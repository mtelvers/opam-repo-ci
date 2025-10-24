open Lwt.Syntax
open Opam_repo_ci
open Tyxml

let log_src = Logs.Src.create "web" ~doc:"Web interface"
module Log = (val Logs.src_log log_src : Logs.LOG)

(* Configuration *)
type config = {
  port : int;
  db_path : string;
}

let default_config =
  let home = Sys.getenv "HOME" in
  let base_dir = Filename.concat home "opam-ci-slurm" in
  {
    port = 8092;
    db_path = Filename.concat base_dir "db.sqlite";
  }

(* HTML helpers *)
let page ~title:page_title content =
  let open Html in
  html
    (head (title (txt page_title)) [
      meta ~a:[a_charset "UTF-8"] ();
      style ~a:[a_mime_type "text/css"] [
        txt {|
          body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif;
            margin: 0;
            padding: 20px;
            background: #fafafa;
            color: #333;
          }
          h1 {
            font-size: 24px;
            font-weight: 500;
            margin: 20px 0 10px 0;
            color: #000;
          }
          h2 {
            font-size: 18px;
            font-weight: 500;
            margin: 20px 0 10px 0;
            color: #333;
          }
          nav {
            background: white;
            padding: 10px 20px;
            margin: -20px -20px 20px -20px;
            border-bottom: 1px solid #ddd;
          }
          nav a {
            margin-right: 20px;
            color: #0366d6;
            text-decoration: none;
            font-size: 14px;
          }
          nav a:hover { text-decoration: underline; }

          table {
            border-collapse: collapse;
            width: 100%;
            background: white;
            margin: 20px 0;
            border: 1px solid #ddd;
            font-size: 14px;
          }
          th, td {
            padding: 8px 12px;
            text-align: left;
            border-bottom: 1px solid #eee;
          }
          th {
            background: #fafafa;
            font-weight: 600;
            color: #555;
            font-size: 12px;
            text-transform: uppercase;
            letter-spacing: 0.5px;
          }
          tr:last-child td { border-bottom: none; }
          tbody tr:hover { background: #f6f8fa; }

          .status-pending, .status-submitted {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #d1d5da;
            color: #24292f;
          }
          .status-running {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #0969da;
            color: white;
          }
          .status-success {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #1a7f37;
            color: white;
          }
          .status-no-solution {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #fb8500;
            color: white;
          }
          .status-dep-failed {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #9a6700;
            color: white;
          }
          .status-failure {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #cf222e;
            color: white;
          }
          .status-internal-failure {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #82071e;
            color: white;
          }
          .status-cancelled {
            display: inline-block;
            padding: 2px 6px;
            border-radius: 3px;
            font-size: 12px;
            font-weight: 500;
            background: #656d76;
            color: white;
          }

          .pr-link {
            color: #0366d6;
            text-decoration: none;
            font-weight: 500;
          }
          .pr-link:hover { text-decoration: underline; }

          .summary {
            background: white;
            padding: 20px;
            margin: 20px 0;
            border: 1px solid #ddd;
            border-radius: 3px;
          }
          .summary p {
            margin: 5px 0;
            color: #555;
            font-size: 14px;
          }
          .summary-item {
            display: inline-block;
            margin-right: 30px;
          }
          .summary-number {
            font-size: 32px;
            font-weight: 600;
            color: #000;
          }
          .summary-label {
            color: #666;
            font-size: 14px;
          }

          .retry-btn {
            background: #2da44e;
            color: white;
            border: 1px solid rgba(27, 31, 36, 0.15);
            padding: 5px 16px;
            border-radius: 6px;
            cursor: pointer;
            font-size: 14px;
            font-weight: 500;
            margin: 10px 0;
          }
          .retry-btn:hover {
            background: #2c974b;
          }

          pre {
            background: #f6f8fa;
            color: #24292f;
            padding: 16px;
            border: 1px solid #d0d7de;
            border-radius: 6px;
            overflow-x: auto;
            font-family: ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, "Liberation Mono", monospace;
            font-size: 12px;
            line-height: 1.5;
            white-space: pre-wrap;
            word-wrap: break-word;
            margin: 16px 0;
          }

          .tree-view {
            background: white;
            border: 1px solid #ddd;
            border-radius: 3px;
            margin: 20px 0;
          }
          .package-group {
            border-bottom: 1px solid #eee;
          }
          .package-group:last-child {
            border-bottom: none;
          }
          .package-header {
            padding: 12px 16px;
            background: #fafafa;
            font-weight: 600;
            font-size: 14px;
            color: #000;
            border-bottom: 1px solid #eee;
          }
          .job-list {
            padding: 0;
            margin: 0;
            list-style: none;
          }
          .job-item {
            padding: 10px 16px 10px 32px;
            border-bottom: 1px solid #f6f8fa;
            display: flex;
            align-items: center;
            font-size: 13px;
          }
          .job-item:hover {
            background: #f6f8fa;
          }
          .job-item:last-child {
            border-bottom: none;
          }
          .job-variant {
            flex: 0 0 240px;
            font-family: ui-monospace, monospace;
            color: #555;
          }
          .job-status {
            flex: 0 0 100px;
          }
          .job-slurm-id {
            flex: 0 0 100px;
            font-family: ui-monospace, monospace;
            font-size: 11px;
            color: #666;
          }
          .job-exit-code {
            flex: 0 0 80px;
            color: #666;
          }
          .job-actions {
            flex: 1;
            text-align: right;
          }
          .job-actions a {
            margin-left: 8px;
            color: #0366d6;
            text-decoration: none;
            font-size: 12px;
          }
          .job-actions a:hover {
            text-decoration: underline;
          }
          .job-actions .retry-btn {
            margin: 0 0 0 8px;
            padding: 3px 10px;
            font-size: 12px;
          }
        |}
      ];
    ])
    (body [
      nav [
        a ~a:[a_href "/"] [txt "PRs"];
        a ~a:[a_href "/jobs"] [txt "Jobs"];
      ];
      div content
    ])

(* Determine CSS class based on status and exit code *)
let status_class status exit_code =
  match status, exit_code with
  | "completed", Some 0 -> "status-success"
  | "completed", Some 1 -> "status-no-solution"
  | "completed", Some 2 -> "status-dep-failed"
  | "completed", Some 3 -> "status-failure"
  | "failed", _ -> "status-internal-failure"
  | "cancelled", _ -> "status-cancelled"
  | "running", _ -> "status-running"
  | "pending", _ | "submitted", _ -> "status-pending"
  | _ -> "status-pending"

(* Determine display text based on status and exit code *)
let status_text status exit_code =
  match status, exit_code with
  | "completed", Some 0 -> "Success"
  | "completed", Some 1 -> "No solution"
  | "completed", Some 2 -> "Dependency failed"
  | "completed", Some 3 -> "Failure"
  | "failed", _ -> "Internal failure"
  | "cancelled", _ -> "Cancelled"
  | "running", _ -> "Running"
  | "pending", _ -> "Pending"
  | "submitted", _ -> "Submitted"
  | _ -> status

(* PRs page - shows all PRs *)
let prs_page db =
  Log.info (fun f -> f "Fetching all PRs");
  let* prs = Db.get_recent_prs db ~limit:1000 in
  Log.info (fun f -> f "Found %d PRs" (List.length prs));

  let pr_row (pr : Db.pr) =
    let open Html in
    tr [
      td [a ~a:[a_href (Printf.sprintf "/pr/%d" pr.Db.pr_number); a_class ["pr-link"]]
            [txt (Printf.sprintf "#%d" pr.Db.pr_number)]];
      td [txt (String.sub pr.Db.commit_hash 0 (min 8 (String.length pr.Db.commit_hash)))];
      td ~a:[a_class [status_class pr.Db.status None]] [txt (status_text pr.Db.status None)];
      td [txt (Printf.sprintf "%d / %d" pr.Db.completed_jobs pr.Db.total_jobs)];
      td [txt (if pr.Db.failed_jobs > 0 then Printf.sprintf "%d" pr.Db.failed_jobs else "-")];
    ] in

  let content = Html.[
    h1 [txt "Pull Requests"];
    div ~a:[a_class ["summary"]] [
      div ~a:[a_class ["summary-item"]] [
        div ~a:[a_class ["summary-number"]] [txt (string_of_int (List.length prs))];
        div ~a:[a_class ["summary-label"]] [txt "Total PRs"];
      ];
    ];
    tablex ~thead:(thead [
        tr [
          th [txt "PR"];
          th [txt "Commit"];
          th [txt "Status"];
          th [txt "Progress"];
          th [txt "Failed"];
        ]
      ]) [
      tbody (List.map pr_row prs);
    ];
  ] in

  Lwt.return (page ~title:"Pull Requests" content)

(* Visual status indicator *)
let status_icon status exit_code =
  let open Html in
  match status, exit_code with
  | "completed", Some 0 -> span ~a:[a_style "color: #1a7f37;"] [txt "✓"]
  | "completed", Some 1 -> span ~a:[a_style "color: #fb8500;"] [txt "○"]
  | "completed", Some 2 -> span ~a:[a_style "color: #9a6700;"] [txt "◐"]
  | "completed", Some 3 -> span ~a:[a_style "color: #cf222e;"] [txt "✗"]
  | "failed", _ -> span ~a:[a_style "color: #82071e;"] [txt "!"]
  | "cancelled", _ -> span ~a:[a_style "color: #656d76;"] [txt "−"]
  | "running", _ -> span ~a:[a_style "color: #0969da;"] [txt "⏳"]
  | "pending", _ | "submitted", _ -> span ~a:[a_style "color: #d1d5da;"] [txt "⏱"]
  | _ -> span ~a:[a_style "color: #d1d5da;"] [txt "?"]

(* PR detail page *)
let pr_page db pr_number =
  let* pr_opt = Db.get_pr db pr_number in
  match pr_opt with
  | None ->
      let content = Html.[
        h1 [txt (Printf.sprintf "PR #%d" pr_number)];
        p [txt "Not found"];
      ] in
      Lwt.return (page ~title:(Printf.sprintf "PR #%d" pr_number) content)
  | Some pr ->
      let* jobs = Db.get_jobs_by_pr db pr_number in

      (* Group jobs by ocaml_version -> package -> arch *)
      let jobs_by_compiler =
        List.fold_left (fun acc (job : Db.job) ->
          let existing = try List.assoc job.ocaml_version acc with Not_found -> [] in
          (job.ocaml_version, job :: existing) :: List.remove_assoc job.ocaml_version acc
        ) [] jobs
        |> List.map (fun (ocaml, jobs) -> (ocaml, List.rev jobs))
        |> List.sort (fun (a, _) (b, _) -> String.compare a b)
      in

      (* Render a single arch job with status icon *)
      let arch_item (job : Db.job) =
        Html.(
          li ~a:[a_style "display: inline-block; margin-right: 12px;"] [
            status_icon job.status job.exit_code;
            txt " ";
            (* Internal failures have no log, so don't link them *)
            if job.status = "failed" then
              span ~a:[a_style "color: #656d76;"] [txt job.arch]
            else
              a ~a:[a_href (Printf.sprintf "/job/%d/logs" job.id);
                    a_style "color: #0366d6; text-decoration: none;"] [txt job.arch];
          ]
        )
      in

      (* Group jobs within a compiler by package, then render package with its archs *)
      let package_item (package, package_jobs) =
        Html.(
          li ~a:[a_style "margin: 4px 0;"] [
            strong ~a:[a_style "color: #24292f;"] [txt package];
            ul ~a:[a_style "list-style: none; padding-left: 20px; margin: 2px 0;"]
              (List.map arch_item package_jobs);
          ]
        )
      in

      (* Render a compiler version with its packages *)
      let compiler_section (ocaml_version, compiler_jobs) =
        (* Group by package within this compiler *)
        let jobs_by_package =
          List.fold_left (fun acc (job : Db.job) ->
            let existing = try List.assoc job.package acc with Not_found -> [] in
            (job.package, job :: existing) :: List.remove_assoc job.package acc
          ) [] compiler_jobs
          |> List.map (fun (pkg, jobs) -> (pkg, List.rev jobs))
          |> List.sort (fun (a, _) (b, _) -> String.compare a b)
        in
        Html.(
          div ~a:[a_style "margin: 16px 0;"] [
            h3 ~a:[a_style "margin: 8px 0; font-size: 15px; font-weight: 600; color: #24292f;"]
              [txt ocaml_version];
            ul ~a:[a_style "list-style: none; padding-left: 20px; margin: 4px 0;"]
              (List.map package_item jobs_by_package);
          ]
        )
      in

      let content = Html.[
        h1 [txt (Printf.sprintf "PR #%d" pr_number)];
        div ~a:[a_class ["summary"]] [
          p [txt (Printf.sprintf "Commit: %s" pr.commit_hash)];
          p [txt (Printf.sprintf "Status: %s" pr.status)];
          p [txt (Printf.sprintf "Progress: %d / %d jobs completed" pr.completed_jobs pr.total_jobs)];
          p [txt (Printf.sprintf "Failed: %d" pr.failed_jobs)];
        ];
        h2 [txt "Compilers"];
        div (List.map compiler_section jobs_by_compiler);
      ] in

      Lwt.return (page ~title:(Printf.sprintf "PR #%d" pr_number) content)

(* Jobs page - shows all running jobs *)
let jobs_page db =
  Log.info (fun f -> f "Fetching running jobs");
  let* jobs = Db.get_running_jobs db in
  Log.info (fun f -> f "Found %d running jobs" (List.length jobs));

  (* Count jobs by status *)
  let running_count = List.filter (fun (j : Db.job) -> j.status = "running") jobs |> List.length in

  let job_row (job : Db.job) =
    let open Html in
    let slurm_id = match job.slurm_job_id with
      | Some id -> id
      | None -> "-"
    in
    tr [
      td [a ~a:[a_href (Printf.sprintf "/pr/%d" job.pr_number); a_class ["pr-link"]]
            [txt (Printf.sprintf "#%d" job.pr_number)]];
      td [txt job.package];
      td [txt job.arch];
      td [txt job.ocaml_version];
      td ~a:[a_class [status_class job.status job.exit_code]]
        [txt (status_text job.status job.exit_code)];
      td [txt slurm_id];
      td [a ~a:[a_href (Printf.sprintf "/job/%d/logs" job.id)] [txt "logs"]];
    ] in

  let content = Html.[
    h1 [txt "Running Jobs"];
    div ~a:[a_class ["summary"]] [
      div ~a:[a_class ["summary-item"]] [
        div ~a:[a_class ["summary-number"]] [txt (string_of_int (List.length jobs))];
        div ~a:[a_class ["summary-label"]] [txt "Total Jobs"];
      ];
      div ~a:[a_class ["summary-item"]] [
        div ~a:[a_class ["summary-number"]] [txt (string_of_int running_count)];
        div ~a:[a_class ["summary-label"]] [txt "Running"];
      ];
    ];
    tablex ~thead:(thead [
        tr [
          th [txt "PR"];
          th [txt "Package"];
          th [txt "Arch"];
          th [txt "OCaml"];
          th [txt "Status"];
          th [txt "Slurm ID"];
          th [txt "Logs"];
        ]
      ]) [
      tbody (List.map job_row jobs);
    ];
  ] in

  Lwt.return (page ~title:"Running Jobs" content)

(* HTTP request handler *)
let handle_request db _conn req _body =
  let uri = Cohttp.Request.uri req in
  let path = Uri.path uri in
  let meth = Cohttp.Request.meth req in

  Log.info (fun f -> f "%s %s" (Cohttp.Code.string_of_method meth) path);

  match meth, String.split_on_char '/' path with
  (* GET routes *)
  | `GET, path_parts -> begin match path_parts with
  | "" :: [] | "" :: "" :: [] ->
      let* html = prs_page db in
      let body = Format.asprintf "%a" (Html.pp ()) html in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK
        ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()

  | "" :: "jobs" :: [] ->
      let* html = jobs_page db in
      let body = Format.asprintf "%a" (Html.pp ()) html in
      Cohttp_lwt_unix.Server.respond_string ~status:`OK
        ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()

  | "" :: "pr" :: pr_str :: [] ->
      begin try
        let pr_number = int_of_string pr_str in
        let* html = pr_page db pr_number in
        let body = Format.asprintf "%a" (Html.pp ()) html in
        Cohttp_lwt_unix.Server.respond_string ~status:`OK
          ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()
      with _ ->
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"Invalid PR number" ()
      end

  | "" :: "job" :: job_id_str :: "logs" :: [] ->
      begin try
        let job_id = int_of_string job_id_str in
        let* job_opt = Db.get_job db job_id in
        match job_opt with
        | None ->
            let content = Html.[
              h1 [txt "Job Log"];
              p [txt "Job not found"];
            ] in
            let* html = Lwt.return (page ~title:"Job Log" content) in
            let body = Format.asprintf "%a" (Html.pp ()) html in
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
              ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()
        | Some job ->
            match job.output_file with
            | None ->
                let content = Html.[
                  h1 [txt (Printf.sprintf "Job #%d Log" job_id)];
                  p [txt "No log file available"];
                ] in
                let* html = Lwt.return (page ~title:"Job Log" content) in
                let body = Format.asprintf "%a" (Html.pp ()) html in
                Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                  ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()
            | Some file_path ->
                Lwt.catch
                  (fun () ->
                    let* log_content = Lwt_io.with_file ~mode:Lwt_io.input file_path
                      (fun ic -> Lwt_io.read ic) in

                    let retry_button = Html.(
                      form ~a:[a_method `Post; a_action (Printf.sprintf "/job/%d/retry" job_id)] [
                        button ~a:[a_button_type `Submit; a_class ["retry-btn"]] [txt "Retry Job"]
                      ]
                    ) in

                    let content = Html.[
                      h1 [txt (Printf.sprintf "Job #%d: %s (%s %s)" job_id job.package job.arch job.ocaml_version)];
                      retry_button;
                      h2 [txt "Build Log"];
                      pre [txt log_content];
                    ] in
                    let* html = Lwt.return (page ~title:(Printf.sprintf "Job #%d Log" job_id) content) in
                    let body = Format.asprintf "%a" (Html.pp ()) html in
                    Cohttp_lwt_unix.Server.respond_string ~status:`OK
                      ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()
                  )
                  (fun _exn ->
                    let content = Html.[
                      h1 [txt (Printf.sprintf "Job #%d Log" job_id)];
                      p [txt (Printf.sprintf "Could not read log file: %s" file_path)];
                    ] in
                    let* html = Lwt.return (page ~title:"Job Log" content) in
                    let body = Format.asprintf "%a" (Html.pp ()) html in
                    Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
                      ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()
                  )
      with _ ->
        let content = Html.[
          h1 [txt "Job Log"];
          p [txt "Invalid job ID"];
        ] in
        let* html = Lwt.return (page ~title:"Job Log" content) in
        let body = Format.asprintf "%a" (Html.pp ()) html in
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found
          ~headers:(Cohttp.Header.init_with "content-type" "text/html") ~body ()
      end

  | _ ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"Not found" ()
  end

  (* POST routes *)
  | `POST, ("" :: "job" :: job_id_str :: "retry" :: []) ->
      begin try
        let job_id = int_of_string job_id_str in
        let* job_opt = Db.get_job db job_id in
        match job_opt with
        | None ->
            Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"Job not found" ()
        | Some job ->
            (* Call the service API to retry the job *)
            let uri = Uri.of_string (Printf.sprintf "http://localhost:8091/retry/%d" job_id) in
            let* resp, body = Cohttp_lwt_unix.Client.post uri in
            let status = Cohttp.Response.status resp in

            (* Always consume the body to avoid leaking streams *)
            let* body_str = Cohttp_lwt.Body.to_string body in

            if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then begin
              Log.info (fun f -> f "Successfully requested retry for job %d" job_id);
              (* Redirect back to PR page *)
              let headers = Cohttp.Header.init_with "location" (Printf.sprintf "/pr/%d" job.pr_number) in
              Cohttp_lwt_unix.Server.respond ~status:`Found ~headers ~body:Cohttp_lwt.Body.empty ()
            end else begin
              Log.err (fun f -> f "Failed to retry job %d: %s" job_id body_str);
              Cohttp_lwt_unix.Server.respond_string ~status:`Internal_server_error
                ~body:(Printf.sprintf "Failed to retry job: %s" body_str) ()
            end
      with _ ->
        Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"Invalid job ID" ()
      end

  | _ ->
      Cohttp_lwt_unix.Server.respond_string ~status:`Method_not_allowed ~body:"Method not allowed" ()

(* Main entry point *)
let main config =
  Log.info (fun f -> f "Starting OPAM CI Web Interface");
  Log.info (fun f -> f "Port: %d" config.port);
  Log.info (fun f -> f "Database: %s" config.db_path);

  (* Initialize database *)
  let* db = Db.init config.db_path in

  (* Start HTTP server *)
  Log.info (fun f -> f "Starting HTTP server on port %d" config.port);
  let callback = handle_request db in
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
     "PORT HTTP server port (default: 8092)");
    ("--db", Arg.String (fun db -> config := { !config with db_path = db }),
     "PATH Database file path");
    ("--verbose", Arg.Unit (fun () -> Logs.set_level (Some Logs.Debug)),
     " Enable debug logging");
  ] in

  Arg.parse spec (fun _ -> ()) "OPAM Repository CI - Web Interface";

  Lwt_main.run (main !config)
