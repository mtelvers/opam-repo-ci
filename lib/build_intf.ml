(** Modules to be used to build packages must satisfy this interface *)
module type S = sig
  (** [v ocluster ~label ~spec ~master ~urgent ~base commit] runs the build
    specified by [spec], on top of [base], with the new [commit] as compared
    to the [master] branch. The job is labelled [label]. [urgent] specifies
    whether high- and low-priority jobs are set as urgent. *)
  val v :
    label:string ->
    spec:Spec.t Current.t ->
    base:Spec.base Current.t ->
    master:Current_git.Commit.t Current.t ->
    urgent:([`High | `Low] -> bool) option Current.t ->
    Current_git.Commit_id.t Current.t ->
    unit Current.t

  (** [list_revdeps ~platform ~opam_version ~pkgopt ~base ~master ~after commit]
      lists the set of reverse dependencies of the package specified by
      [pkgopt], as modified in [commit] relative to the [master] branch.

      The spec is generated on top of [base], and the OCurrent job is run
      after the job specified by [after], making it a dependency. *)
  val list_revdeps :
    variant:Variant.t ->
    opam_version:[`V2_0 | `V2_1 | `V2_2 | `Dev] ->
    pkgopt:Package_opt.t Current.t ->
    new_pkgs:OpamPackage.t list Current.t ->
    base:Spec.base Current.t ->
    master:Current_git.Commit.t Current.t ->
    after:unit Current.t ->
    Current_git.Commit_id.t Current.t ->
    OpamPackage.Set.t Current.t
end
