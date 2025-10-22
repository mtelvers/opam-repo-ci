(** Slurm job submission and monitoring *)

type job_id = string
(** Slurm job ID *)

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
(** Specification for a build job *)

val submit_build : build_spec -> (job_id, [> `Msg of string ]) result Lwt.t
(** Submit a build job to Slurm. Returns the job ID on success. *)

val get_job_status : job_id -> (job_status, [> `Msg of string ]) result Lwt.t
(** Query the status of a Slurm job *)

val get_job_output : job_id -> (string, [> `Msg of string ]) result Lwt.t
(** Retrieve the output (stdout/stderr) from a completed job *)

val cancel_job : job_id -> (unit, [> `Msg of string ]) result Lwt.t
(** Cancel a running job *)

val get_all_jobs : unit -> (job_id list, [> `Msg of string ]) result Lwt.t
(** Get all jobs submitted by this system *)
