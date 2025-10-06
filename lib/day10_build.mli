(** Day10 build backend - replaces OCluster with day10 health-check tool *)

(** OCaml compiler versions to test against *)
val ocaml_versions : string list

(** Configuration for day10 builds *)
type t

(** [config ~cache_dir ?ssh_hosts ~pool_size ()] creates a day10 build configuration.

    @param cache_dir Directory to use for day10 caching
    @param ssh_hosts Optional list of (architecture, hostname) pairs for running
                     builds on remote systems via SSH. Empty list means all
                     builds run locally.
    @param pool_size Maximum number of concurrent day10 builds *)
val config : cache_dir:string -> ?ssh_hosts:(Ocaml_version.arch * string) list -> pool_size:int -> unit -> t

(** [v t ~pr_commit ~label ~spec ~master ~urgent ~base commit] runs the build
    specified by [spec], on top of [base], with the new [commit] as compared
    to the [master] branch. The job is labelled [label]. [urgent] specifies
    whether high- and low-priority jobs are set as urgent. The [pr_commit]
    is the already-fetched PR commit to avoid duplicate fetch operations. *)
val v :
  t ->
  pr_commit:Current_git.Commit.t Current.t ->
  label:string ->
  spec:Opam_ci_check.Spec.t Current.t ->
  base:Opam_ci_check.Spec.base Current.t ->
  master:Current_git.Commit.t Current.t ->
  urgent:([`High | `Low] -> bool) option Current.t ->
  Current_git.Commit_id.t Current.t ->
  unit Current.t

(** [list_revdeps ~pr_commit ~platform ~opam_version ~pkgopt ~base ~master ~after commit]
    lists the set of reverse dependencies of the package specified by
    [pkgopt], as modified in [commit] relative to the [master] branch.

    The spec is generated on top of [base], and the OCurrent job is run
    after the job specified by [after], making it a dependency. The [pr_commit]
    is the already-fetched PR commit to avoid duplicate fetch operations. *)
val list_revdeps :
  t ->
  pr_commit:Current_git.Commit.t Current.t ->
  variant:Opam_ci_check.Variant.t ->
  opam_version:Opam_ci_check.Opam_version.t ->
  pkgopt:Package_opt.t Current.t ->
  new_pkgs:OpamPackage.t list Current.t ->
  base:Opam_ci_check.Spec.base Current.t ->
  master:Current_git.Commit.t Current.t ->
  after:unit Current.t ->
  Current_git.Commit_id.t Current.t ->
  OpamPackage.Set.t Current.t
