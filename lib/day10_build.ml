open Current.Syntax
open Lwt.Infix

module Spec = Opam_ci_check.Spec
module Variant = Opam_ci_check.Variant
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

(* Standard paths on build workers *)
let repo_path = "/var/cache/opam-repository"
let cache_dir = "/var/cache/day10"

(* Wrap command for execution via SSH or locally *)
let wrap_exec ~ssh_host cmd =
  match ssh_host with
  | None | Some "localhost" -> ("", [|"sh"; "-c"; cmd|])
  | Some host -> ("", [|"ssh"; host; cmd|])

(* Generate shell script that creates worktree with flock and runs a command *)
let make_worktree_script ~worktree_dir ~pr_hash ~master_hash ~command =
  Printf.sprintf
    "flock /var/lock/day10-git.lock sh -c 'if ! test -d %s; then git -C %s fetch origin %s && git -C %s worktree add -f %s %s && cd %s && git merge --no-edit %s; fi' && cd %s && %s"
    (Filename.quote worktree_dir)
    (Filename.quote repo_path)
    (Filename.quote pr_hash)
    (Filename.quote repo_path)
    (Filename.quote worktree_dir)
    (Filename.quote master_hash)
    (Filename.quote worktree_dir)
    (Filename.quote pr_hash)
    (Filename.quote worktree_dir)
    command

(* OCaml compiler versions to test against - dynamically generated from ocaml-version library *)
let ocaml_versions () =
  Ocaml_version.Releases.(recent @ unreleased_betas)
  |> List.map Ocaml_version.to_string

type t = {
  ssh_hosts: (Ocaml_version.arch * string * unit Current.Pool.t) list;
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

  let build { config; master; pr_commit } job { Key.commit = _; package; ocaml_version; with_tests; arch } =
    let { ssh_hosts } = config in

    (* Find SSH host and pool for this architecture *)
    let ssh_host, pool =
      match List.find_opt (fun (a, _, _) -> a = arch) ssh_hosts with
      | Some (_, host, pool) -> (Some host, pool)
      | None -> (None, List.find_map (fun (_, _, p) -> Some p) ssh_hosts |> Option.get)
    in

    Current.Job.start_with job ~pool ~level:Current.Level.Average >>= fun () ->

    (* Calculate paths and hashes *)
    let pr_hash = Git.Commit.hash pr_commit in
    let master_hash = Git.Commit.hash master in
    let worktree_dir = Printf.sprintf "/tmp/day10-worktree-%s" pr_hash in

    (* Build day10 command *)
    let pkg_full = OpamPackage.to_string package in
    let day10_cmd = [
      "day10"; "health-check";
      "--cache-dir"; cache_dir;
      "--opam-repository"; worktree_dir;
      "--ocaml-version"; "ocaml." ^ ocaml_version;
      "--log";
    ] @ (if with_tests then ["--with-test"] else [])
      @ [pkg_full]
    in

    (* Build the shell command with flock for worktree creation + day10 execution *)
    let shell_script = make_worktree_script
      ~worktree_dir ~pr_hash ~master_hash
      ~command:(String.concat " " (List.map Filename.quote day10_cmd))
    in

    (* Log reproduction instructions *)
    Current.Job.write job
      (Fmt.str "@.To reproduce locally:@.@.%s@.@."
        (String.concat " " day10_cmd));

    (* Execute via SSH or locally *)
    let exec_cmd = wrap_exec ~ssh_host shell_script in

    (* Run command *)
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

let config ssh_hosts_with_pools =
  { ssh_hosts = ssh_hosts_with_pools }

let ssh_hosts t =
  List.map (fun (arch, host, _pool) -> (arch, host)) t.ssh_hosts

let v t ~pr_commit ~label ~spec ~base:_ ~master ~urgent:_ commit =
  Current.component "%s" label |>
  let> { Spec.variant; ty } = spec
  and> commit_id = commit
  and> master
  and> pr_commit in

  (* Extract package and test info from spec *)
  match ty with
  | `Opam (`Build { Spec.with_tests; revdep; _ }, pkg) ->
      (* When testing revdeps, test the revdep package, not the original package *)
      let package = match revdep with
        | Some revdep_pkg -> revdep_pkg  (* Test the reverse dependency *)
        | None -> pkg                     (* Test the original package *)
      in

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
    let { ssh_hosts } = config in

    (* Find SSH host and pool for this architecture *)
    let ssh_host, pool =
      match List.find_opt (fun (a, _, _) -> a = arch) ssh_hosts with
      | Some (_, host, pool) -> (Some host, pool)
      | None -> (None, List.find_map (fun (_, _, p) -> Some p) ssh_hosts |> Option.get)
    in

    Current.Job.start_with job ~pool ~level:Current.Level.Average >>= fun () ->

    (* Calculate paths and hashes *)
    let pr_hash = Git.Commit.hash pr_commit in
    let master_hash = Git.Commit.hash master in
    let worktree_dir = Printf.sprintf "/tmp/day10-worktree-%s" pr_hash in

    (* Run day10 revdeps to get reverse dependencies of the package *)
    let pkg_string = OpamPackage.to_string package in
    let revdeps_cmd = [
      "day10"; "revdeps";
      "--opam-repository"; worktree_dir;
      "--ocaml-version"; "ocaml." ^ ocaml_version;
      pkg_string;
    ] in

    Current.Job.log job "Listing revdeps with: %s" (String.concat " " revdeps_cmd);

    (* Build the shell command with flock for worktree creation + day10 execution *)
    let shell_script = make_worktree_script
      ~worktree_dir ~pr_hash ~master_hash
      ~command:(String.concat " " (List.map Filename.quote revdeps_cmd))
    in

    let exec_cmd = wrap_exec ~ssh_host shell_script in

    (* Execute and capture stdout *)
    Current.Process.check_output ~cancellable:true ~job exec_cmd >>!= fun output ->
    let packages =
      output
      |> String.split_on_char '\n'
      |> List.filter_map (fun line ->
          OpamPackage.of_string_opt (String.trim line)
        )
    in
    Current.Job.log job "Found %d reverse dependencies" (List.length packages);
    Lwt_result.return (OpamPackage.Set.of_list packages)

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
