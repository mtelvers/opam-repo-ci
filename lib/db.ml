open Lwt.Syntax

type t = {
  db : Sqlite3.db;
  mutex : Lwt_mutex.t;
}

type job = {
  id : int;
  pr_number : int;
  commit_hash : string;
  package : string;
  variant : string;
  slurm_job_id : string option;
  status : string;
  exit_code : int option;
  output_file : string option;
  error_file : string option;
  created_at : float;
  updated_at : float;
}

type pr = {
  pr_number : int;
  commit_hash : string;
  status : string;
  total_jobs : int;
  completed_jobs : int;
  failed_jobs : int;
  created_at : float;
  updated_at : float;
}

let log_src = Logs.Src.create "db" ~doc:"Database"
module Log = (val Logs.src_log log_src : Logs.LOG)

(** Execute SQL with error handling *)
let exec db sql =
  Lwt_preemptive.detach (fun () ->
    match Sqlite3.exec db sql with
    | Sqlite3.Rc.OK -> Ok ()
    | rc -> Error (`Msg (Sqlite3.Rc.to_string rc))
  ) ()

(** Prepare and execute query *)
let with_stmt db sql f =
  Lwt_preemptive.detach (fun () ->
    let stmt = Sqlite3.prepare db sql in
    let result = f stmt in
    let _ = Sqlite3.finalize stmt in
    result
  ) ()

(** Initialize database schema *)
let init path =
  let* db = Lwt_preemptive.detach (fun () ->
    let db = Sqlite3.db_open path in
    (* Enable WAL mode for better concurrent access *)
    let _ = Sqlite3.exec db "PRAGMA journal_mode=WAL;" in
    db
  ) () in

  let schema = {|
    CREATE TABLE IF NOT EXISTS jobs (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      pr_number INTEGER NOT NULL,
      commit_hash TEXT NOT NULL,
      package TEXT NOT NULL,
      variant TEXT NOT NULL,
      slurm_job_id TEXT,
      status TEXT NOT NULL DEFAULT 'pending',
      exit_code INTEGER,
      output_file TEXT,
      error_file TEXT,
      created_at REAL NOT NULL,
      updated_at REAL NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_jobs_pr ON jobs(pr_number);
    CREATE INDEX IF NOT EXISTS idx_jobs_commit ON jobs(commit_hash);
    CREATE INDEX IF NOT EXISTS idx_jobs_status ON jobs(status);
    CREATE INDEX IF NOT EXISTS idx_jobs_slurm_id ON jobs(slurm_job_id);

    CREATE TABLE IF NOT EXISTS prs (
      pr_number INTEGER PRIMARY KEY,
      commit_hash TEXT NOT NULL,
      status TEXT NOT NULL DEFAULT 'pending',
      total_jobs INTEGER NOT NULL DEFAULT 0,
      completed_jobs INTEGER NOT NULL DEFAULT 0,
      failed_jobs INTEGER NOT NULL DEFAULT 0,
      created_at REAL NOT NULL,
      updated_at REAL NOT NULL
    );

    CREATE INDEX IF NOT EXISTS idx_prs_updated ON prs(updated_at DESC);
  |} in

  let* result = exec db schema in
  match result with
  | Ok () ->
      Log.info (fun f -> f "Database initialized at %s" path);
      Lwt.return { db; mutex = Lwt_mutex.create () }
  | Error (`Msg msg) ->
      Log.err (fun f -> f "Failed to initialize database: %s" msg);
      Lwt.fail_with msg

let close t =
  Lwt_preemptive.detach (fun () ->
    let _ = Sqlite3.db_close t.db in
    ()
  ) ()

(** Create a new job *)
let create_job t ~pr_number ~commit_hash ~package ~variant ~log_file =
  Lwt_mutex.with_lock t.mutex (fun () ->
    let now = Unix.gettimeofday () in
    let* result = with_stmt t.db
      "INSERT INTO jobs (pr_number, commit_hash, package, variant, output_file, error_file, created_at, updated_at) \
       VALUES (?, ?, ?, ?, ?, ?, ?, ?)"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int pr_number)) in
        let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT commit_hash) in
        let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.TEXT package) in
        let _ = Sqlite3.bind stmt 4 (Sqlite3.Data.TEXT variant) in
        let _ = Sqlite3.bind stmt 5 (Sqlite3.Data.TEXT log_file) in
        let _ = Sqlite3.bind stmt 6 (Sqlite3.Data.NULL) in  (* error_file no longer used *)
        let _ = Sqlite3.bind stmt 7 (Sqlite3.Data.FLOAT now) in
        let _ = Sqlite3.bind stmt 8 (Sqlite3.Data.FLOAT now) in
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> Ok ()
        | rc -> Error (`Msg (Sqlite3.Rc.to_string rc))
      )
    in
    match result with
    | Ok () ->
        let id = Sqlite3.last_insert_rowid t.db |> Int64.to_int in
        Lwt.return id
    | Error (`Msg msg) ->
        Lwt.fail_with msg
  )

(** Update job with Slurm job ID *)
let update_job_submitted t ~job_id ~slurm_job_id =
  Lwt_mutex.with_lock t.mutex (fun () ->
    let* result = with_stmt t.db
      "UPDATE jobs SET slurm_job_id = ?, status = 'submitted', updated_at = ? WHERE id = ?"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT slurm_job_id) in
        let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.FLOAT (Unix.gettimeofday ())) in
        let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int job_id)) in
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> Ok ()
        | rc -> Error (`Msg (Sqlite3.Rc.to_string rc))
      )
    in
    match result with
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) -> Lwt.fail_with msg
  )

(** Update job status *)
let update_job_status t ~job_id ~status ~exit_code =
  Lwt_mutex.with_lock t.mutex (fun () ->
    let* result = with_stmt t.db
      "UPDATE jobs SET status = ?, exit_code = ?, updated_at = ? WHERE id = ?"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT status) in
        let _ = Sqlite3.bind stmt 2 (match exit_code with
          | Some code -> Sqlite3.Data.INT (Int64.of_int code)
          | None -> Sqlite3.Data.NULL) in
        let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.FLOAT (Unix.gettimeofday ())) in
        let _ = Sqlite3.bind stmt 4 (Sqlite3.Data.INT (Int64.of_int job_id)) in
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> Ok ()
        | rc -> Error (`Msg (Sqlite3.Rc.to_string rc))
      )
    in
    match result with
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) -> Lwt.fail_with msg
  )

(** Helper to parse job from row *)
let parse_job row =
  match row with
  | [| Sqlite3.Data.INT id; INT pr; TEXT hash; TEXT pkg; TEXT var;
       slurm_id; TEXT status; exit_code; out_file; err_file;
       FLOAT created; FLOAT updated |] ->
      Some {
        id = Int64.to_int id;
        pr_number = Int64.to_int pr;
        commit_hash = hash;
        package = pkg;
        variant = var;
        slurm_job_id = (match slurm_id with TEXT s -> Some s | _ -> None);
        status;
        exit_code = (match exit_code with INT i -> Some (Int64.to_int i) | _ -> None);
        output_file = (match out_file with TEXT s -> Some s | _ -> None);
        error_file = (match err_file with TEXT s -> Some s | _ -> None);
        created_at = created;
        updated_at = updated;
      }
  | _ -> None

(** Get job by ID *)
let get_job t id =
  Lwt_mutex.with_lock t.mutex (fun () ->
    with_stmt t.db
      "SELECT id, pr_number, commit_hash, package, variant, slurm_job_id, status, \
       exit_code, output_file, error_file, created_at, updated_at FROM jobs WHERE id = ?"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int id)) in
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let row = Sqlite3.row_data stmt in
            parse_job row
        | _ -> None
      )
  )

(** Get jobs by PR *)
let get_jobs_by_pr t pr_number =
  Lwt_mutex.with_lock t.mutex (fun () ->
    with_stmt t.db
      "SELECT id, pr_number, commit_hash, package, variant, slurm_job_id, status, \
       exit_code, output_file, error_file, created_at, updated_at FROM jobs WHERE pr_number = ?"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int pr_number)) in
        let rec collect acc =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let row = Sqlite3.row_data stmt in
              (match parse_job row with
               | Some job -> collect (job :: acc)
               | None -> collect acc)
          | _ -> List.rev acc
        in
        collect []
      )
  )

(** Get jobs by commit *)
let get_jobs_by_commit t commit_hash =
  Lwt_mutex.with_lock t.mutex (fun () ->
    with_stmt t.db
      "SELECT id, pr_number, commit_hash, package, variant, slurm_job_id, status, \
       exit_code, output_file, error_file, created_at, updated_at FROM jobs WHERE commit_hash = ?"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.TEXT commit_hash) in
        let rec collect acc =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let row = Sqlite3.row_data stmt in
              (match parse_job row with
               | Some job -> collect (job :: acc)
               | None -> collect acc)
          | _ -> List.rev acc
        in
        collect []
      )
  )

(** Get pending jobs *)
let get_pending_jobs t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    with_stmt t.db
      "SELECT id, pr_number, commit_hash, package, variant, slurm_job_id, status, \
       exit_code, output_file, error_file, created_at, updated_at FROM jobs WHERE status = 'pending'"
      (fun stmt ->
        let rec collect acc =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let row = Sqlite3.row_data stmt in
              (match parse_job row with
               | Some job -> collect (job :: acc)
               | None -> collect acc)
          | _ -> List.rev acc
        in
        collect []
      )
  )

(** Get running jobs *)
let get_running_jobs t =
  Lwt_mutex.with_lock t.mutex (fun () ->
    with_stmt t.db
      "SELECT id, pr_number, commit_hash, package, variant, slurm_job_id, status, \
       exit_code, output_file, error_file, created_at, updated_at FROM jobs \
       WHERE status IN ('submitted', 'running')"
      (fun stmt ->
        let rec collect acc =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let row = Sqlite3.row_data stmt in
              (match parse_job row with
               | Some job -> collect (job :: acc)
               | None -> collect acc)
          | _ -> List.rev acc
        in
        collect []
      )
  )

(** Create or update PR *)
let create_or_update_pr t ~pr_number ~commit_hash ~total_jobs =
  Lwt_mutex.with_lock t.mutex (fun () ->
    let now = Unix.gettimeofday () in
    let* result = with_stmt t.db
      "INSERT INTO prs (pr_number, commit_hash, total_jobs, created_at, updated_at) \
       VALUES (?, ?, ?, ?, ?) \
       ON CONFLICT(pr_number) DO UPDATE SET \
         commit_hash = excluded.commit_hash, \
         total_jobs = excluded.total_jobs, \
         updated_at = excluded.updated_at"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int pr_number)) in
        let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.TEXT commit_hash) in
        let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.INT (Int64.of_int total_jobs)) in
        let _ = Sqlite3.bind stmt 4 (Sqlite3.Data.FLOAT now) in
        let _ = Sqlite3.bind stmt 5 (Sqlite3.Data.FLOAT now) in
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> Ok ()
        | rc -> Error (`Msg (Sqlite3.Rc.to_string rc))
      )
    in
    match result with
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) -> Lwt.fail_with msg
  )

(** Update PR statistics *)
let update_pr_stats t pr_number =
  Lwt_mutex.with_lock t.mutex (fun () ->
    let* result = with_stmt t.db
      "UPDATE prs SET \
         completed_jobs = (SELECT COUNT(*) FROM jobs WHERE pr_number = ? AND status = 'completed'), \
         failed_jobs = (SELECT COUNT(*) FROM jobs WHERE pr_number = ? AND status IN ('failed', 'cancelled')), \
         status = CASE \
           WHEN (SELECT COUNT(*) FROM jobs WHERE pr_number = ? AND status IN ('pending', 'submitted', 'running')) > 0 THEN 'running' \
           WHEN (SELECT COUNT(*) FROM jobs WHERE pr_number = ? AND status = 'failed') > 0 THEN 'failed' \
           ELSE 'completed' \
         END, \
         updated_at = ? \
       WHERE pr_number = ?"
      (fun stmt ->
        let pr64 = Int64.of_int pr_number in
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT pr64) in
        let _ = Sqlite3.bind stmt 2 (Sqlite3.Data.INT pr64) in
        let _ = Sqlite3.bind stmt 3 (Sqlite3.Data.INT pr64) in
        let _ = Sqlite3.bind stmt 4 (Sqlite3.Data.INT pr64) in
        let _ = Sqlite3.bind stmt 5 (Sqlite3.Data.FLOAT (Unix.gettimeofday ())) in
        let _ = Sqlite3.bind stmt 6 (Sqlite3.Data.INT pr64) in
        match Sqlite3.step stmt with
        | Sqlite3.Rc.DONE -> Ok ()
        | rc -> Error (`Msg (Sqlite3.Rc.to_string rc))
      )
    in
    match result with
    | Ok () -> Lwt.return_unit
    | Error (`Msg msg) -> Lwt.fail_with msg
  )

(** Parse PR from row *)
let parse_pr row =
  match row with
  | [| Sqlite3.Data.INT pr; TEXT hash; TEXT status; INT total; INT completed; INT failed;
       FLOAT created; FLOAT updated |] ->
      Some {
        pr_number = Int64.to_int pr;
        commit_hash = hash;
        status;
        total_jobs = Int64.to_int total;
        completed_jobs = Int64.to_int completed;
        failed_jobs = Int64.to_int failed;
        created_at = created;
        updated_at = updated;
      }
  | _ -> None

(** Get PR by number *)
let get_pr t pr_number =
  Lwt_mutex.with_lock t.mutex (fun () ->
    with_stmt t.db
      "SELECT pr_number, commit_hash, status, total_jobs, completed_jobs, failed_jobs, \
       created_at, updated_at FROM prs WHERE pr_number = ?"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int pr_number)) in
        match Sqlite3.step stmt with
        | Sqlite3.Rc.ROW ->
            let row = Sqlite3.row_data stmt in
            parse_pr row
        | _ -> None
      )
  )

(** Get recent PRs *)
let get_recent_prs t ~limit =
  Lwt_mutex.with_lock t.mutex (fun () ->
    with_stmt t.db
      "SELECT pr_number, commit_hash, status, total_jobs, completed_jobs, failed_jobs, \
       created_at, updated_at FROM prs ORDER BY pr_number ASC LIMIT ?"
      (fun stmt ->
        let _ = Sqlite3.bind stmt 1 (Sqlite3.Data.INT (Int64.of_int limit)) in
        let rec collect acc =
          match Sqlite3.step stmt with
          | Sqlite3.Rc.ROW ->
              let row = Sqlite3.row_data stmt in
              (match parse_pr row with
               | Some pr -> collect (pr :: acc)
               | None -> collect acc)
          | _ -> List.rev acc
        in
        collect []
      )
  )
