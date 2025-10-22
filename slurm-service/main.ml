open Lwt.Syntax
open Opam_repo_ci

let log_src = Logs.Src.create "main" ~doc:"Main service"
module Log = (val Logs.src_log log_src : Logs.LOG)

(* Configuration *)
type config = {
  port : int;
  opam_repo_url : string;
  opam_repo_path : string;
  cache_dir : string;
  work_dir : string;
  db_path : string;
  _github_webhook_secret : string option;
  poll_interval : float;
  github_token : string option;
  github_repo_owner : string;
  github_repo_name : string;
}

let default_config =
  let home = Sys.getenv "HOME" in
  let base_dir = Filename.concat home "opam-ci-slurm" in
  {
    port = 8091;
    opam_repo_url = "https://github.com/ocaml/opam-repository.git";
    opam_repo_path = Filename.concat base_dir "opam-repository";
    cache_dir = Filename.concat base_dir "cache";
    work_dir = Filename.concat base_dir "work";
    db_path = Filename.concat base_dir "db.sqlite";
    _github_webhook_secret = None;
    poll_interval = 30.0;
    github_token = None;
    github_repo_owner = "ocaml";
    github_repo_name = "opam-repository";
  }

(* GitHub webhook payload types *)
type github_pr_action =
  | Opened
  | Reopened
  | Synchronize
  | Other of string

type github_pr_event = {
  action : github_pr_action;
  number : int;
  sha : string;
}

(* Parse GitHub webhook payload *)
let parse_pr_action = function
  | "opened" -> Opened
  | "reopened" -> Reopened
  | "synchronize" -> Synchronize
  | other -> Other other

let parse_pr_event body =
  try
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    let action = json |> member "action" |> to_string |> parse_pr_action in
    let number = json |> member "number" |> to_int in
    let sha = json |> member "pull_request" |> member "head" |> member "sha" |> to_string in
    Some { action; number; sha }
  with _ ->
    None

(* HTTP handlers *)
let handle_github_webhook config db_promise body =
  match parse_pr_event body with
  | None ->
      Log.warn (fun f -> f "Failed to parse GitHub webhook payload");
      Lwt.return (`String "Invalid payload", 400)
  | Some event ->
      begin match event.action with
      | Opened | Reopened | Synchronize ->
          Log.info (fun f -> f "Received PR #%d event (action=%s, sha=%s)"
            event.number
            (match event.action with
             | Opened -> "opened"
             | Reopened -> "reopened"
             | Synchronize -> "synchronize"
             | Other s -> s)
            event.sha);

          (* Process PR asynchronously *)
          Lwt.async (fun () ->
            let* db = db_promise in
            let coord_config : Coordinator.config = {
              opam_repo_url = config.opam_repo_url;
              opam_repo_path = config.opam_repo_path;
              cache_dir = config.cache_dir;
              work_dir = config.work_dir;
              db;
            } in

            let* result = Coordinator.process_pr coord_config
              ~pr_number:event.number
              ~commit_hash:event.sha
            in
            match result with
            | Ok () ->
                Log.info (fun f -> f "Successfully queued builds for PR #%d" event.number);
                Lwt.return_unit
            | Error (`Msg msg) ->
                Log.err (fun f -> f "Failed to process PR #%d: %s" event.number msg);
                Lwt.return_unit
          );

          Lwt.return (`String "OK", 200)
      | Other action ->
          Log.info (fun f -> f "Ignoring PR #%d action: %s" event.number action);
          Lwt.return (`String "Ignored", 200)
      end

(* Poll GitHub for recent open PRs *)
let poll_github_prs config db =
  Log.info (fun f -> f "Polling GitHub for recent open PRs...");

  let uri = Uri.of_string
    (Printf.sprintf "https://api.github.com/repos/%s/%s/pulls?state=open&per_page=100"
      config.github_repo_owner config.github_repo_name)
  in

  let auth_header = match config.github_token with
    | Some token -> [("Authorization", "token " ^ token)]
    | None -> []
  in

  let headers = Cohttp.Header.of_list (
    auth_header @ [
      ("Accept", "application/vnd.github.v3+json");
      ("User-Agent", "opam-ci-slurm");
    ]
  ) in

  begin match config.github_token with
  | Some _ -> Log.info (fun f -> f "Using authenticated GitHub API (5000 req/hr)")
  | None -> Log.info (fun f -> f "Using unauthenticated GitHub API (60 req/hr)")
  end;

  Lwt.catch
    (fun () ->
      let* resp, body = Cohttp_lwt_unix.Client.get ~headers uri in
      let status = Cohttp.Response.status resp in

      if not (Cohttp.Code.is_success (Cohttp.Code.code_of_status status)) then begin
        Log.err (fun f -> f "GitHub API error: %d" (Cohttp.Code.code_of_status status));
        Lwt.return_unit
      end else begin
        let* body_str = Cohttp_lwt.Body.to_string body in

        try
          let json = Yojson.Safe.from_string body_str in
          let open Yojson.Safe.Util in
          let prs = json |> to_list in

          Log.info (fun f -> f "Found %d open PRs on GitHub" (List.length prs));

          (* TEMPORARY: Only process first PR to avoid traffic *)
          let prs_to_process = match prs with
            | first :: _ ->
                Log.info (fun f -> f "Processing only first PR (temporary limit)");
                [first]
            | [] -> []
          in

          (* Process each PR *)
          Lwt_list.iter_s (fun pr_json ->
            try
              let pr_number = pr_json |> member "number" |> to_int in
              let sha = pr_json |> member "head" |> member "sha" |> to_string in

              (* Check if we already have this PR with this commit *)
              let* existing_pr = Db.get_pr db pr_number in
              match existing_pr with
              | Some pr when pr.commit_hash = sha ->
                  (* Already processed this commit *)
                  Lwt.return_unit
              | _ ->
                  (* New PR or new commit, process it *)
                  Log.info (fun f -> f "Processing PR #%d (sha=%s) from GitHub poll"
                    pr_number (String.sub sha 0 8));

                  let coord_config : Coordinator.config = {
                    opam_repo_path = config.opam_repo_path;
                    opam_repo_url = config.opam_repo_url;
                    cache_dir = config.cache_dir;
                    work_dir = config.work_dir;
                    db;
                  } in

                  Lwt.catch
                    (fun () ->
                      let* result = Coordinator.process_pr coord_config ~pr_number ~commit_hash:sha in
                      match result with
                      | Ok () -> Lwt.return_unit
                      | Error (`Msg msg) ->
                          Log.err (fun f -> f "Failed to process PR #%d: %s" pr_number msg);
                          Lwt.return_unit
                    )
                    (fun exn ->
                      Log.err (fun f -> f "Failed to process PR #%d: %s"
                        pr_number (Printexc.to_string exn));
                      Lwt.return_unit
                    )
            with exn ->
              Log.warn (fun f -> f "Failed to parse PR from GitHub response: %s"
                (Printexc.to_string exn));
              Lwt.return_unit
          ) prs_to_process
        with exn ->
          Log.err (fun f -> f "Failed to parse GitHub API response: %s"
            (Printexc.to_string exn));
          Lwt.return_unit
      end
    )
    (fun exn ->
      Log.err (fun f -> f "GitHub API request failed: %s" (Printexc.to_string exn));
      Lwt.return_unit
    )

(* Simple HTTP server *)
let handle_request config db_promise _conn req body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in

  Log.info (fun f -> f "%s %s" (Cohttp.Code.string_of_method meth) path);

  match meth, path with
  | `POST, "/webhook/github" ->
      let* body_str = Cohttp_lwt.Body.to_string body in
      let* response, code = handle_github_webhook config db_promise body_str in
      let body_str = match response with `String s -> s in
      let headers = Cohttp.Header.init_with "content-type" "text/plain" in
      Cohttp_lwt_unix.Server.respond_string ~status:(Cohttp.Code.status_of_code code)
        ~headers ~body:body_str ()

  | `GET, "/health" ->
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"OK" ()

  | `GET, "/" ->
      let body = "OPAM Repository CI - Slurm Edition\n\n\
                  POST /webhook/github - GitHub webhook endpoint\n\
                  POST /retry/{job_id} - Retry a failed job\n\
                  GET  /health         - Health check\n" in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body ()

  | `POST, path when String.starts_with ~prefix:"/retry/" path ->
      begin try
        let job_id = int_of_string (String.sub path 7 (String.length path - 7)) in
        let* db = db_promise in
        let* job_opt = Db.get_job db job_id in
        match job_opt with
        | None ->
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"Job not found" ()
        | Some job ->
            (* Get PR info *)
            let* pr_opt = Db.get_pr db job.pr_number in
            begin match pr_opt with
            | None ->
                Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"PR not found" ()
            | Some _pr ->
                (* Calculate worktree path *)
                let base_dir = Filename.dirname config.opam_repo_path in
                let worktree_path = Filename.concat (Filename.concat base_dir "worktrees")
                  (Printf.sprintf "pr%d" job.pr_number) in

                (* Parse variant to get arch and ocaml_version *)
                begin match String.split_on_char '-' job.variant with
                | arch :: ocaml_version :: _ ->
                    (* Create Slurm build spec *)
                    let spec : Slurm.build_spec = {
                      pr_number = job.pr_number;
                      commit_hash = job.commit_hash;
                      package = job.package;
                      arch;
                      ocaml_version;
                      opam_repo_path = worktree_path;
                      cache_dir = config.cache_dir;
                      work_dir = config.work_dir;
                    } in

                    (* Submit to Slurm *)
                    let* submit_result = Slurm.submit_build spec in
                    begin match submit_result with
                    | Ok slurm_job_id ->
                        (* Update job record *)
                        let* () = Db.update_job_submitted db ~job_id ~slurm_job_id in
                        Log.info (fun f -> f "Retried job %d as Slurm job %s" job_id slurm_job_id);
                        Cohttp_lwt_unix.Server.respond_string ~status:`OK ~body:"Job retried successfully" ()
                    | Error (`Msg msg) ->
                        Log.err (fun f -> f "Failed to retry job %d: %s" job_id msg);
                        Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error
                          ~body:(Printf.sprintf "Failed to submit job: %s" msg) ()
                    end
                | _ ->
                    Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request
                      ~body:"Invalid variant format" ()
                end
            end
      with _ ->
        Cohttp_lwt_unix.Server.respond_string ~status:`Bad_request ~body:"Invalid job ID" ()
      end

  | _ ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"Not found" ()

(* Initialize directories *)
let init_directories config =
  let dirs = [
    config.opam_repo_path;
    config.work_dir;
    Filename.dirname config.db_path;
  ] in

  let rec create_dir path =
    Lwt.catch
      (fun () ->
        let* () = Lwt_unix.mkdir path 0o755 in
        Log.info (fun f -> f "Created directory: %s" path);
        Lwt.return_unit
      )
      (function
        | Unix.Unix_error (Unix.EEXIST, _, _) -> Lwt.return_unit
        | Unix.Unix_error (Unix.ENOENT, _, _) ->
            (* Parent doesn't exist, create it first *)
            let parent = Filename.dirname path in
            if parent <> path && parent <> "/" then
              let* () = create_dir parent in
              create_dir path
            else
              Lwt.return_unit
        | exn -> Lwt.fail exn
      )
  in

  Lwt_list.iter_s create_dir dirs

(* Clone opam-repository if needed *)
let init_opam_repo config =
  let* exists = Lwt_unix.file_exists config.opam_repo_path in
  if exists then begin
    (* Check if it's a valid git repository *)
    let git_dir = Filename.concat config.opam_repo_path ".git" in
    let* is_git_repo = Lwt_unix.file_exists git_dir in

    if is_git_repo then begin
      Log.info (fun f -> f "OPAM repository already exists at %s" config.opam_repo_path);
      Lwt.return_unit
    end else begin
      Log.warn (fun f -> f "Directory exists but is not a git repo, removing: %s" config.opam_repo_path);
      let* status = Lwt_process.exec ("rm", [| "rm"; "-rf"; config.opam_repo_path |]) in
      match status with
      | Unix.WEXITED 0 ->
          Log.info (fun f -> f "Cloning OPAM repository to %s..." config.opam_repo_path);
          let command = ("git", [| "git"; "clone"; "--depth"; "1";
                                  config.opam_repo_url; config.opam_repo_path |]) in
          let* status = Lwt_process.exec command in
          begin match status with
          | Unix.WEXITED 0 ->
              Log.info (fun f -> f "Repository cloned successfully");
              Lwt.return_unit
          | _ ->
              Log.err (fun f -> f "Failed to clone repository");
              Lwt.fail_with "Failed to clone opam-repository"
          end
      | _ ->
          Log.err (fun f -> f "Failed to remove invalid directory");
          Lwt.fail_with "Failed to remove invalid opam-repository directory"
    end
  end else begin
    Log.info (fun f -> f "Cloning OPAM repository to %s..." config.opam_repo_path);
    let command = ("git", [| "git"; "clone"; "--depth"; "1";
                            config.opam_repo_url; config.opam_repo_path |]) in
    let* status = Lwt_process.exec command in
    match status with
    | Unix.WEXITED 0 ->
        Log.info (fun f -> f "Repository cloned successfully");
        Lwt.return_unit
    | _ ->
        Log.err (fun f -> f "Failed to clone repository");
        Lwt.fail_with "Failed to clone opam-repository"
  end

(* Main entry point *)
let main config =
  Log.info (fun f -> f "Starting OPAM CI Slurm Service");
  Log.info (fun f -> f "Port: %d" config.port);
  Log.info (fun f -> f "Database: %s" config.db_path);
  Log.info (fun f -> f "OPAM repo: %s" config.opam_repo_path);

  (* Initialize directories and repository *)
  let* () = init_directories config in
  let* () = init_opam_repo config in

  (* Initialize database *)
  let db_promise = Db.init config.db_path in
  let* db = db_promise in

  (* Start job monitor in background *)
  let monitor_config : Monitor.config = {
    db;
    poll_interval = config.poll_interval;
  } in
  Lwt.async (fun () ->
    Lwt.catch
      (fun () -> Monitor.start monitor_config)
      (fun exn ->
        Log.err (fun f -> f "Monitor crashed: %s" (Printexc.to_string exn));
        Lwt.return_unit
      )
  );

  (* Poll GitHub for recent PRs on startup *)
  let* () = poll_github_prs config db in

  (* Start HTTP server *)
  Log.info (fun f -> f "Starting HTTP server on port %d" config.port);
  let callback = handle_request config (Lwt.return db) in
  let server =
    Cohttp_lwt_unix.Server.create
      ~mode:(`TCP (`Port config.port))
      (Cohttp_lwt_unix.Server.make ~callback ())
  in

  server

(* Command-line arguments *)
let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);

  let config = ref default_config in

  let spec = [
    ("--port", Arg.Int (fun p -> config := { !config with port = p }),
     "PORT HTTP server port (default: 8091)");
    ("--opam-repo-url", Arg.String (fun url -> config := { !config with opam_repo_url = url }),
     "URL OPAM repository URL");
    ("--opam-repo-path", Arg.String (fun path -> config := { !config with opam_repo_path = path }),
     "PATH Local path to opam-repository");
    ("--cache-dir", Arg.String (fun dir -> config := { !config with cache_dir = dir }),
     "DIR Cache directory");
    ("--work-dir", Arg.String (fun dir -> config := { !config with work_dir = dir }),
     "DIR Work directory for job outputs");
    ("--db", Arg.String (fun db -> config := { !config with db_path = db }),
     "PATH Database file path");
    ("--poll-interval", Arg.Float (fun i -> config := { !config with poll_interval = i }),
     "SECONDS Job status polling interval (default: 30.0)");
    ("--github-token", Arg.String (fun t -> config := { !config with github_token = Some t }),
     "TOKEN GitHub API token for polling PRs");
    ("--github-repo", Arg.String (fun r ->
       match String.split_on_char '/' r with
       | [owner; name] -> config := { !config with github_repo_owner = owner; github_repo_name = name }
       | _ -> failwith "Invalid repo format, use 'owner/name'"),
     "REPO GitHub repository in format 'owner/name' (default: ocaml/opam-repository)");
    ("--verbose", Arg.Unit (fun () -> Logs.set_level (Some Logs.Debug)),
     " Enable debug logging");
  ] in

  Arg.parse spec (fun _ -> ()) "OPAM Repository CI - Slurm Edition";

  (* Try to get GitHub token from environment if not set via command line *)
  let final_config =
    match !config.github_token with
    | Some _ -> !config
    | None ->
        begin match Sys.getenv_opt "GITHUB_TOKEN" with
        | Some token -> { !config with github_token = Some token }
        | None -> !config
        end
  in

  Lwt_main.run (main final_config)
