open Current.Syntax
open Lwt.Infix

module Spec = Opam_ci_check.Spec
module Variant = Opam_ci_check.Variant
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

(* Cache of (PR commit hash, host) -> worktree path
   This allows multiple builds for the same PR to share a worktree.
   The mutex serializes git worktree operations. *)
module Worktree_cache = struct
  let cache = Hashtbl.create 100
  let lock = Lwt_mutex.create ()

  let get_or_create ~job ~master ~pr_commit ~ssh_host =
    let pr_hash = Git.Commit.hash pr_commit in
    let cache_key = (pr_hash, ssh_host) in
    (* Treat "localhost" as local execution *)
    let is_local = match ssh_host with
      | None -> true
      | Some "localhost" -> true
      | Some _ -> false
    in
    Lwt_mutex.with_lock lock (fun () ->
      match Hashtbl.find_opt cache cache_key with
      | Some path ->
          Current.Job.log job "Reusing existing worktree for PR %s" pr_hash;
          Lwt.return path
      | None ->
          Current.Job.log job "Creating new worktree for PR %s" pr_hash;
          begin match is_local with
          | true ->
              (* Local execution *)
              let repo_path = Git.Commit.repo master in
              let repo_path_str = Fpath.to_string repo_path in
              let worktree_dir = Filename.temp_file "day10-worktree-" ("-" ^ pr_hash) in
              Unix.unlink worktree_dir;

              (* Create worktree from master *)
              let worktree_cmd = ("", [|"git"; "-C"; repo_path_str; "worktree"; "add"; worktree_dir; Git.Commit.hash master|]) in
              Current.Process.exec ~cancellable:false ~job worktree_cmd >>= fun result1 ->
              begin match result1 with
              | Error (`Msg msg) -> Lwt.fail (Failure msg)
              | Ok () ->
                  (* Merge PR commit *)
                  let worktree_fpath = Fpath.v worktree_dir in
                  let merge_cmd = ("", [|"git"; "merge"; "--no-edit"; pr_hash|]) in
                  Current.Process.exec ~cwd:worktree_fpath ~cancellable:false ~job merge_cmd >>= fun result2 ->
                  begin match result2 with
                  | Ok () ->
                      Hashtbl.add cache cache_key worktree_dir;
                      Lwt.return worktree_dir
                  | Error (`Msg msg) ->
                      Current.Job.log job "Failed to merge PR commit";
                      (* Clean up worktree *)
                      let _ = Sys.command (Printf.sprintf "git -C %s worktree remove --force %s" repo_path_str worktree_dir) in
                      Lwt.fail (Failure msg)
                  end
              end

          | false ->
              (* Remote execution via SSH *)
              let host = Option.get ssh_host in
              let repo_path = "/var/cache/opam-repository" in
              let worktree_dir = Printf.sprintf "/tmp/day10-worktree-%s" pr_hash in

              (* Fetch PR commit on remote machine *)
              let fetch_cmd_str = Printf.sprintf "git -C %s fetch origin %s"
                (Filename.quote repo_path) (Filename.quote pr_hash) in
              let fetch_cmd = ("", [|"ssh"; host; fetch_cmd_str|]) in
              Current.Process.exec ~cancellable:false ~job fetch_cmd >>= fun result0 ->
              begin match result0 with
              | Error (`Msg msg) -> Lwt.fail (Failure msg)
              | Ok () ->
                  (* Create worktree from master on remote machine *)
                  let master_hash = Git.Commit.hash master in
                  let worktree_cmd_str = Printf.sprintf "git -C %s worktree add %s %s"
                    (Filename.quote repo_path) (Filename.quote worktree_dir) (Filename.quote master_hash) in
                  let worktree_cmd = ("", [|"ssh"; host; worktree_cmd_str|]) in
                  Current.Process.exec ~cancellable:false ~job worktree_cmd >>= fun result1 ->
                  begin match result1 with
                  | Error (`Msg msg) ->
                      Current.Job.log job "Failed to create worktree";
                      Lwt.fail (Failure msg)
                  | Ok () ->
                      (* Merge PR commit on remote machine *)
                      let merge_cmd_str = Printf.sprintf "cd %s && git merge --no-edit %s"
                        (Filename.quote worktree_dir) (Filename.quote pr_hash) in
                      let merge_cmd = ("", [|"ssh"; host; merge_cmd_str|]) in
                      Current.Process.exec ~cancellable:false ~job merge_cmd >>= fun result2 ->
                      begin match result2 with
                      | Ok () ->
                          Hashtbl.add cache cache_key worktree_dir;
                          Lwt.return worktree_dir
                      | Error (`Msg msg) ->
                          Current.Job.log job "Failed to merge PR commit";
                          (* Clean up worktree on remote machine *)
                          let cleanup_cmd_str = Printf.sprintf "git -C %s worktree remove --force %s"
                            (Filename.quote repo_path) (Filename.quote worktree_dir) in
                          let _ = Sys.command (Printf.sprintf "ssh %s %s" host cleanup_cmd_str) in
                          Lwt.fail (Failure msg)
                      end
                  end
              end
          end
    )
end

(* OCaml compiler versions to test against *)
let ocaml_versions = [
  "4.08.1"; "4.09.1"; "4.10.2"; "4.11.2";
  "4.12.1"; "4.13.1"; "4.14.2";
  "5.0.0"; "5.1.1"; "5.2.1"; "5.3.0";
]

type t = {
  cache_dir: string;
  ssh_hosts: (Ocaml_version.arch * string) list;
  pool: unit Current.Pool.t;
}

module Op = struct
  type nonrec t = {
    config : t;
    master : Current_git.Commit.t;
    pr_commit : Current_git.Commit.t;
  }

  let id = "day10-health-check"

  module Key = struct
    type t = {
      commit : Current_git.Commit_id.t;
      package : OpamPackage.t;
      ocaml_version : string;
      with_tests : bool;
      arch : Ocaml_version.arch;
    }

    let to_json { commit; package; ocaml_version; with_tests; arch } =
      `Assoc [
        "commit", `String (Current_git.Commit_id.hash commit);
        "package", `String (OpamPackage.to_string package);
        "ocaml_version", `String ocaml_version;
        "with_tests", `Bool with_tests;
        "arch", `String (Ocaml_version.string_of_arch arch);
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = Current.Unit

  (* Get or create a worktree for this PR (shared across all builds for the same PR) *)
  let get_opam_repo ~job ~master ~pr_commit ~ssh_host =
    Worktree_cache.get_or_create ~job ~master ~pr_commit ~ssh_host >|= fun path ->
    Ok path

  let build { config; master; pr_commit } job { Key.commit = _; package; ocaml_version; with_tests; arch } =
    let { cache_dir; ssh_hosts; pool } = config in
    Current.Job.start_with job ~pool ~level:Current.Level.Average >>= fun () ->

    (* Find SSH host for this architecture *)
    let ssh_host = List.assoc_opt arch ssh_hosts in

    (* Get or create shared worktree for this PR *)
    get_opam_repo ~job ~master ~pr_commit ~ssh_host >>!= fun repo_dir ->

    (* Use remote cache dir for SSH builds (but not localhost) *)
    let is_remote = match ssh_host with
      | None | Some "localhost" -> false
      | Some _ -> true
    in
    let cache_dir = if is_remote then "/var/cache/day10" else cache_dir in

    (* Build day10 command with markdown output to stdout *)
    let pkg_full = OpamPackage.to_string package in
    let base_cmd = [
      "day10"; "health-check";
      "--cache-dir"; cache_dir;
      "--opam-repository"; repo_dir;
      "--ocaml-version"; "ocaml." ^ ocaml_version;
      "--md"; "-";
    ] in
    let base_cmd = if with_tests then base_cmd @ ["--with-test"] else base_cmd in
    let cmd = base_cmd @ [pkg_full] in

    (* Log reproduction instructions *)
    Current.Job.write job
      (Fmt.str "@.To reproduce locally:@.@.%s@.@."
        (String.concat " " cmd));

    (* Execute day10 (locally or via SSH) *)
    let exec_cmd = if is_remote then
      (* SSH execution *)
      let host = Option.get ssh_host in
      let remote_cmd = String.concat " " (List.map Filename.quote cmd) in
      ("", Array.of_list ["ssh"; host; remote_cmd])
    else
      (* Local execution *)
      ("", Array.of_list cmd)
    in

    (* Run day10 - it returns non-zero on failure, and log matchers will
       scan the output (including YAML frontmatter) to classify the error *)
    Current.Process.exec ~cancellable:true ~job exec_cmd >>= function
    | Ok () -> Lwt_result.return ()
    | Error _ as e -> Lwt.return e

  let pp f { Key.commit; package; ocaml_version; with_tests; arch } =
    Fmt.pf f "day10 health-check %s (OCaml %s, %s)%s on %a"
      (OpamPackage.to_string package)
      ocaml_version
      (Ocaml_version.string_of_arch arch)
      (if with_tests then " with tests" else "")
      Current_git.Commit_id.pp commit

  let auto_cancel = true
end

module BC = Current_cache.Make(Op)

let config ~cache_dir ?(ssh_hosts=[]) ~pool_size () =
  let pool = Current.Pool.create ~label:"day10" pool_size in
  { cache_dir; ssh_hosts; pool }

let ssh_hosts t = t.ssh_hosts

let v t ~pr_commit ~label ~spec ~base:_ ~master ~urgent:_ commit =
  Current.component "%s" label |>
  let> { Spec.variant; ty } = spec
  and> commit_id = commit
  and> master
  and> pr_commit in

  (* Extract package and test info from spec *)
  match ty with
  | `Opam (`Build { Spec.with_tests; _ }, pkg) ->
      let package = pkg in

      (* Extract OCaml version and architecture from variant *)
      let ocaml_version = Variant.ocaml_version_to_string variant in
      let arch = Variant.arch variant in

      let t = { Op.config = t; master; pr_commit } in
      BC.get t { Op.Key.commit = commit_id; package; ocaml_version; with_tests; arch }
      |> Current.Primitive.map_result (Result.map ignore)
  | `Opam (`List_revdeps _, _) ->
      (* This should never happen - list_revdeps is called via a different function *)
      failwith "day10_build.v: unexpected List_revdeps type"

(* List reverse dependencies using day10 list command *)
module List_revdeps_op = struct
  type nonrec t = {
    config : t;
    master : Current_git.Commit.t;
    pr_commit : Current_git.Commit.t;
  }

  let id = "day10-list-revdeps"

  module Key = struct
    type t = {
      commit : Current_git.Commit_id.t;
      package : OpamPackage.t;
      ocaml_version : string;
      arch : Ocaml_version.arch;
    }

    let to_json { commit; package; ocaml_version; arch } =
      `Assoc [
        "commit", `String (Current_git.Commit_id.hash commit);
        "package", `String (OpamPackage.to_string package);
        "ocaml_version", `String ocaml_version;
        "arch", `String (Ocaml_version.string_of_arch arch);
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = struct
    type t = OpamPackage.Set.t
    let marshal t = Marshal.to_string t []
    let unmarshal s = Marshal.from_string s 0
  end

  let build { config; master; pr_commit } job { Key.commit = _; package; ocaml_version; arch } =
    let { cache_dir = _; ssh_hosts; pool } = config in
    Current.Job.start_with job ~pool ~level:Current.Level.Average >>= fun () ->

    (* Find SSH host for this architecture *)
    let ssh_host = List.assoc_opt arch ssh_hosts in

    (* Get or create shared worktree for this PR *)
    Op.get_opam_repo ~job ~master ~pr_commit ~ssh_host >>!= fun repo_dir ->

    (* Run day10 revdeps to get reverse dependencies of the package *)
    let output_file = Filename.temp_file "day10-revdeps-" ".txt" in
    let pkg_string = OpamPackage.to_string package in
    let revdeps_cmd = [
      "day10"; "revdeps";
      "--opam-repository"; repo_dir;
      "--ocaml-version"; "ocaml." ^ ocaml_version;
      pkg_string;
    ] in

    Current.Job.log job "Listing revdeps with: %s" (String.concat " " revdeps_cmd);

    (* Execute and capture output *)
    let redirect_cmd = String.concat " " (List.map Filename.quote revdeps_cmd) ^ " > " ^ output_file in

    let is_remote = match ssh_host with
      | None | Some "localhost" -> false
      | Some _ -> true
    in
    let exec_cmd = if is_remote then
      (* SSH execution *)
      let host = Option.get ssh_host in
      ("", [|"ssh"; host; redirect_cmd|])
    else
      (* Local execution *)
      ("", [|"sh"; "-c"; redirect_cmd|])
    in

    Lwt.finalize
      (fun () ->
        Current.Process.exec ~cancellable:true ~job exec_cmd >>!= fun () ->

        (* Read package list *)
        let ic = ref None in
        Lwt.finalize
          (fun () ->
            let ch = open_in output_file in
            ic := Some ch;
            let rec read_lines acc =
              match input_line ch with
              | line ->
                  (* Parse package name from output *)
                  (match OpamPackage.of_string_opt (String.trim line) with
                  | Some pkg -> read_lines (pkg :: acc)
                  | None -> read_lines acc)
              | exception End_of_file -> List.rev acc
            in
            let packages = read_lines [] in

            Current.Job.log job "Found %d reverse dependencies" (List.length packages);

            Lwt_result.return (OpamPackage.Set.of_list packages)
          )
          (fun () ->
            (* Ensure file handle is closed *)
            (match !ic with Some ch -> (try close_in ch with _ -> ()) | None -> ());
            Lwt.return_unit
          )
      )
      (fun () ->
        (* Cleanup output file *)
        (try Unix.unlink output_file with _ -> ());
        Lwt.return_unit
      )

  let pp f { Key.commit; package; ocaml_version; arch } =
    Fmt.pf f "day10 list revdeps of %s (OCaml %s, %s) on %a"
      (OpamPackage.to_string package)
      ocaml_version
      (Ocaml_version.string_of_arch arch)
      Current_git.Commit_id.pp commit

  let auto_cancel = true
end

module List_BC = Current_cache.Make(List_revdeps_op)

let list_revdeps t ~pr_commit ~variant ~opam_version:_ ~pkgopt ~new_pkgs:_ ~base:_ ~master ~after:_ commit =
  Current.component "list revdeps" |>
  let> pkgopt
  and> commit_id = commit
  and> master
  and> pr_commit in

  let package = pkgopt.Package_opt.pkg in
  let ocaml_version = Variant.ocaml_version_to_string variant in
  let arch = Variant.arch variant in

  let t = { List_revdeps_op.config = t; master; pr_commit } in
  List_BC.get t { List_revdeps_op.Key.commit = commit_id; package; ocaml_version; arch }
