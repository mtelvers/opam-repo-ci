(** Build coordinator - orchestrates PR testing *)

type config = {
  opam_repo_url : string;
  opam_repo_path : string;  (** Local path to opam-repository clone *)
  cache_dir : string;        (** Cache directory for day10 *)
  work_dir : string;         (** Working directory for job outputs *)
  db : Db.t;                 (** Database handle *)
}

type build_job = {
  package : string;
  arch : string;
  ocaml_version : string;
}

val process_pr :
  config ->
  pr_number:int ->
  commit_hash:string ->
  (unit, [> `Msg of string ]) result Lwt.t
(** Process a PR: analyze changes, create build matrix, submit jobs *)

val generate_build_matrix :
  packages:string list ->
  build_job list
(** Generate build matrix for given packages *)

val submit_jobs :
  config ->
  pr_number:int ->
  commit_hash:string ->
  worktree_path:string ->
  build_job list ->
  (unit, [> `Msg of string ]) result Lwt.t
(** Create database records and submit jobs to Slurm using worktree *)
