(** GitHub status updates *)

type config = {
  github_token : string;  (** GitHub API token *)
  repo_owner : string;    (** Repository owner (e.g., "ocaml") *)
  repo_name : string;     (** Repository name (e.g., "opam-repository") *)
  status_url : string;    (** Base URL for status links *)
}

type state =
  | Pending
  | Success
  | Failure
  | ErrorState

val update_commit_status :
  config ->
  commit_hash:string ->
  state:state ->
  context:string ->
  description:string ->
  (unit, [> `Msg of string ]) result Lwt.t
(** Update GitHub commit status *)

val update_pr_status :
  config ->
  pr_number:int ->
  commit_hash:string ->
  completed:int ->
  total:int ->
  failed:int ->
  (unit, [> `Msg of string ]) result Lwt.t
(** Update PR status based on job completion *)
