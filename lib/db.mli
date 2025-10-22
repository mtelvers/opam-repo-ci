(** Database for tracking build jobs and PRs *)

type t
(** Database handle *)

type job = {
  id : int;
  pr_number : int;
  commit_hash : string;
  package : string;
  variant : string;  (* arch-ocaml_version *)
  slurm_job_id : string option;
  status : string;  (* pending, running, completed, failed, cancelled *)
  exit_code : int option;
  output_file : string option;
  error_file : string option;
  created_at : float;
  updated_at : float;
}

type pr = {
  pr_number : int;
  commit_hash : string;
  status : string;  (* pending, running, completed, failed, mixed *)
  total_jobs : int;
  completed_jobs : int;
  failed_jobs : int;
  created_at : float;
  updated_at : float;
}

val init : string -> t Lwt.t
(** Initialize database at given path *)

val close : t -> unit Lwt.t
(** Close database connection *)

(** Job operations *)

val create_job :
  t ->
  pr_number:int ->
  commit_hash:string ->
  package:string ->
  variant:string ->
  log_file:string ->
  int Lwt.t
(** Create a new job record. Returns job ID. *)

val update_job_submitted :
  t ->
  job_id:int ->
  slurm_job_id:string ->
  unit Lwt.t
(** Mark job as submitted with Slurm job ID *)

val update_job_status :
  t ->
  job_id:int ->
  status:string ->
  exit_code:int option ->
  unit Lwt.t
(** Update job status *)

val get_job : t -> int -> job option Lwt.t
(** Get job by ID *)

val get_jobs_by_pr : t -> int -> job list Lwt.t
(** Get all jobs for a PR *)

val get_jobs_by_commit : t -> string -> job list Lwt.t
(** Get all jobs for a commit hash *)

val get_pending_jobs : t -> job list Lwt.t
(** Get all jobs with status = pending *)

val get_running_jobs : t -> job list Lwt.t
(** Get all jobs with status = running *)

(** PR operations *)

val create_or_update_pr :
  t ->
  pr_number:int ->
  commit_hash:string ->
  total_jobs:int ->
  unit Lwt.t
(** Create or update PR record *)

val update_pr_stats : t -> int -> unit Lwt.t
(** Recalculate PR statistics from jobs *)

val get_pr : t -> int -> pr option Lwt.t
(** Get PR by number *)

val get_recent_prs : t -> limit:int -> pr list Lwt.t
(** Get recent PRs ordered by updated_at *)
