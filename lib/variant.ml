type arch = Ocaml_version.arch

let arch_to_yojson arch = `String (Ocaml_version.string_of_arch arch)

type t = {
  distribution : string;
  ocaml_version : string;
  ocaml_variant : string option;
  arch : arch;
} [@@deriving to_yojson]

let pp_ocaml_version t =
  let variant = match t.ocaml_variant with
    | None -> ""
    | Some variant -> "-"^variant
  in
  t.ocaml_version^variant

let arch t = t.arch
let docker_tag t =
  t.distribution^"-ocaml-"^pp_ocaml_version t
let distribution t = t.distribution

let pp f t = Fmt.pf f "%s/%s" (docker_tag t) (Ocaml_version.string_of_arch t.arch)

let freebsd = "freebsd"

let windows_distributions = [
  "windows-msvc";
  "windows-mingw";
]

let windows_1809_distributions = [
  "windows-msvc-1809";
  "windows-mingw-1809";
]

let macos_homebrew = "macos-homebrew"

let macos_distributions = [
  macos_homebrew;
  (* TODO: Add macos-macports *)
]

(* TODO: Remove that when macOS uses ocaml-dockerfile *)
let os {distribution; _} =
  if List.exists (String.equal distribution) macos_distributions then
    `macOS
  else if List.exists (String.equal distribution) [ "freebsd" ] then
    `FreeBSD
  else if List.exists (String.equal distribution) windows_distributions then
    `Windows
  else if List.exists (String.equal distribution) windows_1809_distributions then
    `Windows_1809
  else
    `linux

let v ~arch ~distro ~compiler:(ocaml_version, ocaml_variant) =
  { arch; distribution = distro; ocaml_version; ocaml_variant }
