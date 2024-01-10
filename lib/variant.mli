type arch = Ocaml_version.arch

type t [@@deriving to_yojson]

val v : arch:arch -> distro:string -> compiler:(string * string option) -> t

val pp_ocaml_version : t -> string

val arch : t -> arch
val docker_tag : t -> string
val distribution : t -> string
val os : t -> [`linux | `macOS | `FreeBSD | `Windows | `Windows_1809 ]

val pp : t Fmt.t

val macos_homebrew : string
val freebsd : string
val windows_distributions : string list
val windows_1809_distributions : string list
