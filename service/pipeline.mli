(** The main opam-repo-ci pipeline. Tests everything configured for
    GitHub application [app] using the day10 health-check tool. *)
val v :
  day10:Opam_repo_ci.Day10_build.t ->
  app:Current_github.App.t ->
  unit -> unit Current.t

(** [local_test_pr repo branch] is a pipeline that tests branch [branch] on
    the local Git repository at path [repo] using the day10 health-check tool. *)
val local_test_pr : ?test_config:Opam_repo_ci.Integration_test.t -> day10:Opam_repo_ci.Day10_build.t -> Current_git.Local.t -> string -> unit -> unit Current.t
