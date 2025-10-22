(** Job monitor - polls Slurm for job status updates *)

type config = {
  db : Db.t;
  poll_interval : float;  (** Seconds between polls *)
}

val start : config -> unit Lwt.t
(** Start the monitoring loop. This runs forever, polling for job status updates. *)

val check_jobs : Db.t -> (int, [> `Msg of string ]) result Lwt.t
(** Check all running jobs once and update their status. Returns number of jobs updated. *)
