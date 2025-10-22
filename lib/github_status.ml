open Lwt.Syntax

type config = {
  github_token : string;
  repo_owner : string;
  repo_name : string;
  status_url : string;
}

type state =
  | Pending
  | Success
  | Failure
  | ErrorState

let log_src = Logs.Src.create "github_status" ~doc:"GitHub status updates"
module Log = (val Logs.src_log log_src : Logs.LOG)

let state_to_string = function
  | Pending -> "pending"
  | Success -> "success"
  | Failure -> "failure"
  | ErrorState -> "error"

(** Create GitHub API request *)
let github_api_request config ~meth ~path ~body =
  let uri = Uri.of_string
    (Printf.sprintf "https://api.github.com/repos/%s/%s%s"
      config.repo_owner config.repo_name path)
  in

  let headers = Cohttp.Header.of_list [
    ("Authorization", "token " ^ config.github_token);
    ("Accept", "application/vnd.github.v3+json");
    ("User-Agent", "opam-ci-slurm");
  ] in

  Log.debug (fun f -> f "GitHub API %s %s"
    (Cohttp.Code.string_of_method meth)
    (Uri.to_string uri));

  Lwt.catch
    (fun () ->
      let* resp, body = Cohttp_lwt_unix.Client.call ~headers ~body meth uri in
      let status = Cohttp.Response.status resp in
      let* body_str = Cohttp_lwt.Body.to_string body in

      if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then begin
        Log.debug (fun f -> f "GitHub API success: %s" body_str);
        Lwt.return (Ok ())
      end else begin
        Log.err (fun f -> f "GitHub API error %d: %s"
          (Cohttp.Code.code_of_status status) body_str);
        Lwt.return (Error (`Msg (Printf.sprintf "GitHub API error: %d" (Cohttp.Code.code_of_status status))))
      end
    )
    (fun exn ->
      Log.err (fun f -> f "GitHub API exception: %s" (Printexc.to_string exn));
      Lwt.return (Error (`Msg (Printexc.to_string exn)))
    )

(** Update commit status *)
let update_commit_status config ~commit_hash ~state ~context ~description =
  let target_url = Printf.sprintf "%s/pr/%s" config.status_url commit_hash in

  let json = `Assoc [
    ("state", `String (state_to_string state));
    ("target_url", `String target_url);
    ("description", `String description);
    ("context", `String context);
  ] in

  let body = Yojson.Safe.to_string json in
  let path = Printf.sprintf "/statuses/%s" commit_hash in

  Log.info (fun f -> f "Updating status for %s: %s - %s"
    (String.sub commit_hash 0 8) context description);

  github_api_request config
    ~meth:`POST
    ~path
    ~body:(Cohttp_lwt.Body.of_string body)

(** Update PR status based on job completion *)
let update_pr_status config ~pr_number:_ ~commit_hash ~completed ~total ~failed =
  let state, description =
    if completed < total then
      Pending, Printf.sprintf "Testing (%d/%d complete)" completed total
    else if failed > 0 then
      Failure, Printf.sprintf "Failed (%d/%d jobs failed)" failed total
    else
      Success, Printf.sprintf "Passed (%d/%d jobs)" completed total
  in

  update_commit_status config
    ~commit_hash
    ~state
    ~context:"opam-ci-slurm"
    ~description
