open Lwt.Syntax

type config = {
  db : Db.t;
  poll_interval : float;
}

let log_src = Logs.Src.create "monitor" ~doc:"Job monitor"
module Log = (val Logs.src_log log_src : Logs.LOG)

(** Map Slurm status to database status string *)
let slurm_status_to_string = function
  | Slurm.Pending -> "pending"
  | Slurm.Running -> "running"
  | Slurm.Completed -> "completed"
  | Slurm.Failed _ -> "completed"  (* day10 failures are still "completed" with exit code *)
  | Slurm.Cancelled -> "cancelled"
  | Slurm.Unknown -> "unknown"

(** Get exit code from Slurm status *)
let slurm_status_exit_code = function
  | Slurm.Completed -> Some 0
  | Slurm.Failed { exit_code } -> Some exit_code
  | _ -> None

(** Check a single job *)
let check_job db job =
  match job.Db.slurm_job_id with
  | None ->
      Log.warn (fun f -> f "Job %d has no Slurm job ID" job.id);
      Lwt.return (Ok false)
  | Some slurm_job_id ->
      let* status_result = Slurm.get_job_status slurm_job_id in
      match status_result with
      | Error (`Msg msg) ->
          Log.err (fun f -> f "Failed to get status for Slurm job %s: %s" slurm_job_id msg);
          Lwt.return (Ok false)
      | Ok slurm_status ->
          let new_status = slurm_status_to_string slurm_status in
          let exit_code = slurm_status_exit_code slurm_status in

          (* Only update if status changed *)
          if new_status <> job.status then begin
            Log.info (fun f -> f "Job %d status changed: %s -> %s" job.id job.status new_status);
            let* () = Db.update_job_status db ~job_id:job.id ~status:new_status ~exit_code in

            (* Update PR stats if job is terminal *)
            let is_terminal = match slurm_status with
              | Slurm.Completed | Slurm.Failed _ | Slurm.Cancelled -> true
              | _ -> false
            in
            let* () =
              if is_terminal then
                Db.update_pr_stats db job.pr_number
              else
                Lwt.return_unit
            in
            Lwt.return (Ok true)
          end else
            Lwt.return (Ok false)

(** Check all active jobs (submitted to Slurm but not terminal) *)
let check_jobs db =
  let* jobs = Db.get_active_jobs db in
  Log.info (fun f -> f "Checking %d active jobs" (List.length jobs));

  let rec check_all updated = function
    | [] -> Lwt.return (Ok updated)
    | job :: rest ->
        let* result = check_job db job in
        match result with
        | Ok true -> check_all (updated + 1) rest
        | Ok false -> check_all updated rest
        | Error e ->
            Log.err (fun f -> f "Error checking job %d: %s" job.id
              (match e with `Msg s -> s));
            check_all updated rest
  in
  check_all 0 jobs

(** Main monitoring loop *)
let start config =
  Log.info (fun f -> f "Starting job monitor (poll interval: %.1fs)" config.poll_interval);

  let rec loop () =
    let* result = check_jobs config.db in
    begin match result with
    | Ok updated ->
        if updated > 0 then
          Log.info (fun f -> f "Updated %d job(s)" updated)
    | Error (`Msg msg) ->
        Log.err (fun f -> f "Error in monitoring loop: %s" msg)
    end;

    (* Sleep and repeat *)
    let* () = Lwt_unix.sleep config.poll_interval in
    loop ()
  in
  loop ()
