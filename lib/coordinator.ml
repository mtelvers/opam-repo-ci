open Lwt.Syntax

type config = {
  opam_repo_url : string;
  opam_repo_path : string;
  cache_dir : string;
  work_dir : string;
  db : Db.t;
}

type build_job = {
  package : string;
  arch : string;
  ocaml_version : string;
}

let log_src = Logs.Src.create "coordinator" ~doc:"Build coordinator"
module Log = (val Logs.src_log log_src : Logs.LOG)

(** Run git command in repo directory *)
let git_cmd repo_path args =
  let open Lwt_process in
  let command = ("git", Array.of_list ("git" :: "-C" :: repo_path :: args)) in
  Log.info (fun f -> f "Running: git -C %s %s" repo_path (String.concat " " args));

  let* status, stdout =
    with_process_in command (fun proc ->
      let* stdout = Lwt_io.read proc#stdout in
      let* status = proc#status in
      Lwt.return (status, stdout)
    )
  in
  match status with
  | Unix.WEXITED 0 -> Lwt.return (Ok stdout)
  | _ -> Lwt.return (Error (`Msg ("Git command failed: " ^ stdout)))

(** Update repository to latest *)
let update_repo config =
  Log.info (fun f -> f "Updating repository at %s" config.opam_repo_path);
  let* result = git_cmd config.opam_repo_path ["fetch"; "origin"; "master"] in
  match result with
  | Ok _ ->
      Log.info (fun f -> f "Repository updated");
      Lwt.return (Ok ())
  | Error e -> Lwt.return (Error e)

(** Fetch PR ref *)
let fetch_pr config pr_number =
  Log.info (fun f -> f "Fetching PR #%d" pr_number);
  let pr_ref = Printf.sprintf "pull/%d/head" pr_number in
  let* result = git_cmd config.opam_repo_path
    ["fetch"; "origin"; pr_ref]
  in
  match result with
  | Ok _ -> Lwt.return (Ok ())
  | Error e -> Lwt.return (Error e)

(** Create a worktree with PR merged into master *)
let create_pr_worktree config pr_number commit_hash =
  (* Worktree path: base_dir/worktrees/pr{number} *)
  let base_dir = Filename.dirname config.opam_repo_path in
  let worktree_path = Filename.concat (Filename.concat base_dir "worktrees")
                        (Printf.sprintf "pr%d" pr_number) in

  Log.info (fun f -> f "Creating worktree at %s for PR #%d" worktree_path pr_number);

  (* Remove old worktree if it exists *)
  let* () = Lwt.catch
    (fun () ->
      let* _ = git_cmd config.opam_repo_path ["worktree"; "remove"; "--force"; worktree_path] in
      Lwt.return_unit
    )
    (fun _ -> Lwt.return_unit)  (* Ignore errors if worktree doesn't exist *)
  in

  (* Create worktree from master *)
  let* result = git_cmd config.opam_repo_path
    ["worktree"; "add"; worktree_path; "origin/master"]
  in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok _ ->
      (* Merge the PR commit into the worktree *)
      Log.info (fun f -> f "Merging PR commit %s into worktree"
        (String.sub commit_hash 0 (min 8 (String.length commit_hash))));

      let* merge_result = git_cmd worktree_path ["merge"; "--no-edit"; commit_hash] in
      match merge_result with
      | Error e ->
          (* Clean up worktree on merge failure *)
          let* _ = git_cmd config.opam_repo_path ["worktree"; "remove"; "--force"; worktree_path] in
          Lwt.return (Error e)
      | Ok _ ->
          Log.info (fun f -> f "Worktree created and PR merged successfully");
          Lwt.return (Ok worktree_path)

(** Get changed packages in the worktree (which has PR merged into master) *)
let get_changed_packages worktree_path =
  Log.info (fun f -> f "Finding changed packages in worktree %s" worktree_path);

  (* Run diff from within the worktree (which has master merged with PR commit) *)
  (* This shows only the changes from the PR, matching the original behavior *)
  let* result = git_cmd worktree_path
    ["diff"; "--name-only"; "origin/master"; "--"; "packages/"]
  in

  match result with
  | Ok stdout ->
      (* Parse paths like "packages/foo/foo.1.0.0/opam" -> "foo.1.0.0" *)
      let packages =
        String.split_on_char '\n' stdout
        |> List.filter_map (fun line ->
          let line = String.trim line in
          if line = "" then None
          else
            match String.split_on_char '/' line with
            | "packages" :: _pkg_name :: pkg_version :: _ ->
                Some pkg_version
            | _ -> None
        )
        |> List.sort_uniq String.compare
      in
      Log.info (fun f -> f "Found %d changed packages" (List.length packages));
      Lwt.return (Ok packages)
  | Error e -> Lwt.return (Error e)

(** Generate build matrix for packages *)
let generate_build_matrix ~packages =
  (* Define our build matrix *)
  let architectures = [
    "x86_64";
    "arm64";
    "arm32v7";
    "ppc64le";
    "s390x";
    "riscv64";
  ] in

  (* Get OCaml versions from ocaml-version library *)
  let all_supported = Ocaml_version.Releases.recent @ Ocaml_version.Releases.unreleased_betas in
  let ocaml_versions =
    List.map (fun v ->
      "ocaml." ^ Ocaml_version.to_string v
    ) all_supported
  in

  (* Generate all combinations *)
  let jobs = ref [] in
  List.iter (fun package ->
    List.iter (fun arch ->
      List.iter (fun ocaml_version ->
        jobs := {
          package;
          arch;
          ocaml_version;
        } :: !jobs
      ) ocaml_versions
    ) architectures
  ) packages;

  Log.info (fun f -> f "Generated %d build jobs" (List.length !jobs));
  !jobs

(** Submit jobs to Slurm *)
let submit_jobs config ~pr_number ~commit_hash ~worktree_path jobs =
  Log.info (fun f -> f "Submitting %d jobs for PR #%d" (List.length jobs) pr_number);

  (* Create PR record *)
  let* () = Db.create_or_update_pr config.db
    ~pr_number
    ~commit_hash
    ~total_jobs:(List.length jobs)
  in

  (* Submit each job *)
  let rec submit_all = function
    | [] -> Lwt.return (Ok ())
    | job :: rest ->
        let log_file =
          Filename.concat config.work_dir
            (Printf.sprintf "pr%d-%s-%s-%s.log" pr_number job.package job.arch job.ocaml_version)
        in

        (* Create job record *)
        let* job_id = Db.create_job config.db
          ~pr_number
          ~commit_hash
          ~package:job.package
          ~arch:job.arch
          ~ocaml_version:job.ocaml_version
          ~log_file
        in

        (* Submit to Slurm using worktree path *)
        let spec : Slurm.build_spec = {
          pr_number;
          commit_hash;
          package = job.package;
          arch = job.arch;
          ocaml_version = job.ocaml_version;
          opam_repo_path = worktree_path;
          cache_dir = config.cache_dir;
          work_dir = config.work_dir;
        } in

        let* submit_result = Slurm.submit_build spec in
        match submit_result with
        | Ok slurm_job_id ->
            let* () = Db.update_job_submitted config.db ~job_id ~slurm_job_id in
            Log.info (fun f -> f "Submitted job %d as Slurm job %s" job_id slurm_job_id);
            submit_all rest
        | Error (`Msg msg) ->
            Log.err (fun f -> f "Failed to submit job %d: %s" job_id msg);
            (* Mark job as failed (infrastructure/submission error) *)
            let* () = Db.update_job_status config.db ~job_id ~status:"failed" ~exit_code:None in
            (* Continue with other jobs even if one fails *)
            submit_all rest
  in
  submit_all jobs

(** Process a PR: analyze, generate matrix, submit jobs *)
let process_pr config ~pr_number ~commit_hash =
  Log.info (fun f -> f "Processing PR #%d at commit %s" pr_number commit_hash);

  (* Update repository *)
  let* result = update_repo config in
  match result with
  | Error e -> Lwt.return (Error e)
  | Ok () ->
      (* Fetch PR *)
      let* result = fetch_pr config pr_number in
      match result with
      | Error e -> Lwt.return (Error e)
      | Ok () ->
          (* Create worktree with PR merged into master *)
          let* worktree_result = create_pr_worktree config pr_number commit_hash in
          match worktree_result with
          | Error e -> Lwt.return (Error e)
          | Ok worktree_path ->
              (* Get changed packages by diffing from within the worktree *)
              let* result = get_changed_packages worktree_path in
              match result with
              | Error e -> Lwt.return (Error e)
              | Ok [] ->
                  Log.info (fun f -> f "No packages changed in PR #%d" pr_number);
                  Lwt.return (Ok ())
              | Ok packages ->
                  (* Generate build matrix *)
                  let jobs = generate_build_matrix ~packages in

                  (* Submit jobs with worktree path *)
                  submit_jobs config ~pr_number ~commit_hash ~worktree_path jobs
