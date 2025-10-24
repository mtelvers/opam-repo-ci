open Lwt.Syntax

type job_id = string

type job_status =
  | Pending
  | Running
  | Completed
  | Failed of { exit_code : int }
  | Cancelled
  | Unknown

type build_spec = {
  pr_number : int;
  commit_hash : string;
  package : string;
  arch : string;
  ocaml_version : string;
  opam_repo_path : string;
  cache_dir : string;
  work_dir : string;
}

let log_src = Logs.Src.create "slurm" ~doc:"Slurm integration"
module Log = (val Logs.src_log log_src : Logs.LOG)

(** Run a command and return stdout/stderr *)
let run_command ?(timeout=30.0) cmd args =
  let open Lwt_process in
  let command = (cmd, Array.of_list (cmd :: args)) in
  Log.info (fun f -> f "Running: %s %s" cmd (String.concat " " args));

  Lwt.catch
    (fun () ->
      let* status, stdout =
        with_process_in command (fun proc ->
          let* () = Lwt_unix.with_timeout timeout (fun () -> Lwt.return_unit) in
          let* stdout = Lwt_io.read proc#stdout in
          let* status = proc#status in
          Lwt.return (status, stdout)
        )
      in
      match status with
      | Unix.WEXITED 0 -> Lwt.return (Ok stdout)
      | Unix.WEXITED n ->
          Log.err (fun f -> f "Command failed with exit code %d: %s" n stdout);
          Lwt.return (Error (`Msg (Printf.sprintf "Exit code %d: %s" n stdout)))
      | Unix.WSIGNALED s ->
          Lwt.return (Error (`Msg (Printf.sprintf "Killed by signal %d" s)))
      | Unix.WSTOPPED s ->
          Lwt.return (Error (`Msg (Printf.sprintf "Stopped by signal %d" s)))
    )
    (fun exn ->
      Log.err (fun f -> f "Command exception: %s" (Printexc.to_string exn));
      Lwt.return (Error (`Msg (Printexc.to_string exn)))
    )

(** Map architecture names to Slurm constraint names *)
let arch_to_constraint = function
  | "arm64" -> "aarch64"
  | "arm32v7" -> "armv7l"
  | arch -> arch  (* x86_64, ppc64le, s390x, riscv64 remain unchanged *)

(** Generate a unique job name *)
let job_name spec =
  Printf.sprintf "pr%d-%s-%s-%s"
    spec.pr_number
    spec.package
    spec.arch
    spec.ocaml_version

(** Submit a build job to Slurm *)
let submit_build spec =
  let name = job_name spec in
  let log_file = Filename.concat spec.work_dir (name ^ ".log") in

  (* Create work directory if needed *)
  let* () =
    Lwt.catch
      (fun () ->
        let* _ = Lwt_unix.mkdir spec.work_dir 0o755 in
        Lwt.return_unit
      )
      (function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        | exn -> Lwt.fail exn
      )
  in

  (* Build the day10 command *)
  let day10_cmd = Printf.sprintf
    "day10 health-check --log --arch %s --ocaml-version %s --cache-dir %s --opam-repository %s %s"
    spec.arch
    spec.ocaml_version
    spec.cache_dir
    spec.opam_repo_path
    spec.package
  in

  (* Submit via sbatch - stdout and stderr both go to the same log file *)
  let sbatch_args = [
    "--job-name=" ^ name;
    "--output=" ^ log_file;
    "--partition=compute";
    "--constraint=" ^ (arch_to_constraint spec.arch);
    "--mem=30G";
    "--cpus-per-task=4";
    "--wrap=" ^ day10_cmd;
  ] in

  let* result = run_command "sbatch" sbatch_args in
  match result with
  | Ok stdout ->
      (* Parse job ID from "Submitted batch job 12345" *)
      begin match String.split_on_char ' ' (String.trim stdout) with
      | "Submitted" :: "batch" :: "job" :: job_id :: _ ->
          Log.info (fun f -> f "Submitted job %s for %s" job_id name);
          Lwt.return (Ok job_id)
      | _ ->
          Lwt.return (Error (`Msg ("Failed to parse sbatch output: " ^ stdout)))
      end
  | Error e -> Lwt.return (Error e)

(** Parse sacct output to get job status *)
let parse_job_status state exit_code =
  (* Exit code format from sacct is "exitcode:signal", e.g., "2:0" *)
  let code = match String.split_on_char ':' exit_code with
    | code_str :: _ -> (try int_of_string code_str with _ -> 1)
    | [] -> 1
  in
  match String.uppercase_ascii (String.trim state) with
  | "PENDING" | "PD" -> Pending
  | "RUNNING" | "R" -> Running
  | "COMPLETED" | "CD" -> Completed
  | "FAILED" | "F" | "TIMEOUT" | "TO" | "OUT_OF_MEMORY" | "OOM" | "BOOT_FAIL" | "BF" | "NODE_FAIL" | "NF" ->
      Failed { exit_code = code }
  | "CANCELLED" | "CA" | "CANCELLED+" | "PREEMPTED" | "PR" -> Cancelled
  | _ -> Unknown

(** Get job status using sacct *)
let get_job_status job_id =
  let* result = run_command "sacct"
    ["-j"; job_id; "--format=State,ExitCode"; "--noheader"; "--parsable2"]
  in
  match result with
  | Ok stdout ->
      begin match String.split_on_char '|' (String.trim stdout) with
      | state :: exit_code :: _ ->
          let status = parse_job_status state exit_code in
          Lwt.return (Ok status)
      | _ ->
          Log.warn (fun f -> f "Could not parse sacct output: %s" stdout);
          Lwt.return (Ok Unknown)
      end
  | Error e -> Lwt.return (Error e)

(** Get job output from the output files *)
let get_job_output _job_id =
  (* Note: This assumes we know the job name to find the output file *)
  (* In practice, we'll store the output file path in the database *)
  Lwt.return (Error (`Msg "get_job_output needs to be called with output file path"))

(** Cancel a running job *)
let cancel_job job_id =
  let* result = run_command "scancel" [job_id] in
  match result with
  | Ok _ ->
      Log.info (fun f -> f "Cancelled job %s" job_id);
      Lwt.return (Ok ())
  | Error e -> Lwt.return (Error e)

(** Get all jobs for this user *)
let get_all_jobs () =
  let* result = run_command "squeue"
    ["--name=opam-ci-*"; "--format=%i"; "--noheader"]
  in
  match result with
  | Ok stdout ->
      let jobs =
        String.split_on_char '\n' stdout
        |> List.map String.trim
        |> List.filter (fun s -> s <> "")
      in
      Lwt.return (Ok jobs)
  | Error e -> Lwt.return (Error e)
