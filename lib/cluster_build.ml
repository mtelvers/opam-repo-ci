open Current.Syntax
open Capnp_rpc_lwt
open Lwt.Infix

module Variant = Opam_ci_check.Variant
module Spec = Opam_ci_check.Spec
module Opam_build = Opam_ci_check.Opam_build
module Git = Current_git

let ( >>!= ) = Lwt_result.bind

type t = {
  connection : Current_ocluster.Connection.t;
  timeout : Duration.t;
}

let tail ?buffer ~job build_job =
  let rec aux start =
    Cluster_api.Job.log build_job start >>= function
    | Error (`Capnp e) -> Lwt.return @@ Fmt.error_msg "%a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Lwt_result.return ()
    | Ok (data, next) ->
      Stdlib.Option.iter (fun b -> Buffer.add_string b data) buffer;
      Current.Job.write job data;
      aux next
  in aux 0L

let run_job ?buffer ~job build_job =
  let on_cancel _ =
    Cluster_api.Job.cancel build_job >|= function
    | Ok () -> ()
    | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
  in
  Current.Job.with_handler job ~on_cancel @@ fun () ->
  let result = Cluster_api.Job.result build_job in
  tail ?buffer ~job build_job >>!= fun () ->
  result >>= function
  | Error (`Capnp e) -> Lwt_result.fail (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
  | Ok _ as x -> Lwt.return x

let pool_of_variant v =
  (* Temporary hack: override pool if CI_POOL is set *)
  match Sys.getenv_opt "CI_POOL" with
  | Some pool -> pool
  | None ->
      let os = match Variant.os v with
        | `Macos -> "macos"
        | `Freebsd -> "freebsd"
        | `Linux -> "linux"
      in
      let arch = match Variant.arch v with
        | `X86_64 | `I386 -> "x86_64"
        | `Aarch32 | `Aarch64 -> "arm64"
        | `Ppc64le -> "ppc64"
        | `S390x -> "s390x"
        | `Riscv64 -> "riscv64"
      in
      os^"-"^arch

module Op = struct
  type nonrec t = {
    config : t;
    master : Current_git.Commit.t;
    urgent : ([`High | `Low] -> bool) option;
    base : Spec.base;
  }

  let id = "ci-ocluster-build"

  module Key = struct
    type t = {
      pool : string;                            (* The build pool to use (e.g. "linux-arm64") *)
      commit : Current_git.Commit_id.t;         (* The source code to build and test *)
      variant : Variant.t;                      (* Added as a comment in the Dockerfile and selects personality *)
      ty : Spec.ty;
    }

    let to_json { pool; commit; variant; ty } =
      `Assoc [
        "pool", `String pool;
        "commit", `String (Current_git.Commit_id.hash commit);
        "variant", Variant.to_yojson variant;
        "ty", Spec.ty_to_yojson ty;
      ]

    let digest t = Yojson.Safe.to_string (to_json t)
  end

  module Value = Current.String

  (* Helper to check if a line looks like a valid package name.version *)
  let is_valid_package_line line =
    let line = String.trim line in
    if line = "" then false
    else
      try
        let _ = OpamPackage.of_string line in
        true
      with _ -> false

  let parse_output ty job build_job =
    let buffer =
      match ty with
      | `Opam (`List_revdeps _, _) -> Some (Buffer.create 1024)
      | _ -> None
    in
    Capability.with_ref build_job (run_job ?buffer ~job) >>!= fun (_ : string) ->
    match buffer with
    | None -> Lwt_result.return ""
    | Some buffer ->
      (* Parse output by extracting valid opam package names from each line *)
      let lines = String.split_on_char '\n' (Buffer.contents buffer) in
      let packages = List.filter is_valid_package_line lines in
      Lwt_result.return (String.concat "\n" packages)

  (* Helper to create Day10 payload *)
  let day10_payload ~sub_command ~package_name ~ocaml_version ~with_test builder =
    let open Cluster_api.Raw.Builder in
    let day10 = Day10.init_pointer builder in
    Day10.sub_command_set day10 sub_command;
    Day10.package_name_set day10 package_name;
    Day10.ocaml_version_set day10 ocaml_version;
    Day10.with_test_set day10 with_test

  let build { config; master; urgent; base } job
      { Key.pool; commit; variant; ty } =
    let { connection; timeout } = config in
    let master = Current_git.Commit.hash master in
    let timeout = match Variant.arch variant with
      | `Riscv64 -> Int64.mul timeout 2L
      | _ -> timeout in

    (* Extract Day10 job parameters from the build specification *)
    let (sub_command, package_name, with_test) = match ty with
      | `Opam (`Build { revdep = Some revdep; with_tests; _ }, _pkg) ->
          (* When testing a revdep, test the revdep package, not the original package *)
          ("health-check", OpamPackage.to_string revdep, with_tests)
      | `Opam (`Build { revdep = None; with_tests; _ }, pkg) ->
          (* When testing the package itself *)
          ("health-check", OpamPackage.to_string pkg, with_tests)
      | `Opam (`List_revdeps _, pkg) ->
          ("list", OpamPackage.to_string pkg, false)
    in
    let commit_sha = Git.Commit_id.hash commit in
    let ocaml_version = "ocaml." ^ Variant.ocaml_version_to_string variant in

    Current.Job.write job
      (Fmt.str "@.\
                Day10 job:@.@.\
                Sub-command: %s@.\
                Package: %s@.\
                OCaml version: %s@.\
                Commit SHA: %s@.\
                With test: %b@.@."
         sub_command package_name ocaml_version commit_sha with_test);

    (* Create Day10 custom action *)
    let action = Cluster_api.Submission.custom_build @@
      Cluster_api.Custom.v ~kind:"day10" (day10_payload ~sub_command ~package_name ~ocaml_version ~with_test)
    in

    let src = (Git.Commit_id.repo commit, [master; commit_sha]) in
    let cache_hint =
      let pkg =
        match ty with
        | `Opam (`Build { revdep = Some revdep; _ }, pkg) -> Fmt.str "%s-%s" (OpamPackage.to_string pkg) (OpamPackage.to_string revdep)
        | `Opam (`List_revdeps _, pkg)
        | `Opam (`Build _, pkg) -> OpamPackage.to_string pkg
      in
      Fmt.str "%s-%s-%s" (Spec.base_to_string base) pkg commit_sha
    in
    Current.Job.log job "Using cache hint %S" cache_hint;
    Current.Job.log job "Using Day10 job: sub_command=%s package=%s ocaml_version=%s" sub_command package_name ocaml_version;
    let build_pool = Current_ocluster.Connection.pool ?urgent ~job ~pool ~action ~cache_hint ~src connection in
    Current.Job.start_with ~pool:build_pool job ~timeout ~level:Current.Level.Average >>=
    parse_output ty job

  let pp f { Key.pool = _; commit; variant; ty } =
    Fmt.pf f "@[<v>%a@,from %a@,on %a@]"
      Spec.pp_ty ty
      Current_git.Commit_id.pp commit
      Variant.pp variant

  let auto_cancel = true
end

module BC = Current_cache.Make(Op)

let config ~timeout sr =
  let connection = Current_ocluster.Connection.create sr in
  { connection; timeout }

let v t ~label ~spec ~base ~master ~urgent commit =
  Current.component "%s" label |>
  let> { Spec.variant; ty } = spec
  and> base
  and> commit
  and> master
  and> urgent in
  let pool = pool_of_variant variant in
  let t = { Op.config = t; master; urgent; base } in
  BC.get t { Op.Key.pool; commit; variant; ty }
  |> Current.Primitive.map_result (Result.map ignore) (* TODO: Create a separate type of cache that doesn't parse the output *)

let list_revdeps t ~variant ~opam_version ~pkgopt ~new_pkgs ~base ~master ~after commit =
  Current.component "list revdeps" |>
  let> {Package_opt.pkg; urgent; has_tests = _} = pkgopt
  and> new_pkgs
  and> base
  and> commit
  and> master
  and> () = after in
  let pool = pool_of_variant variant in
  let t = { Op.config = t; master; urgent; base } in
  let ty = `Opam (`List_revdeps {Spec.opam_version}, pkg) in
  BC.get t { Op.Key.pool; commit; variant; ty }
  |> Current.Primitive.map_result (Result.map (Common.revdeps ~pkg ~new_pkgs))
