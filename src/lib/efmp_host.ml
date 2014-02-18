open Efmp_internal_pervasives

module Unique_id = Unique_id
module Path = Path
module Configuration = Efmp_configuration
module Process = Efmp_unix_process

open Efmp_job_script

type t = host

type connection = Efmp_data_t.connection

let create ?playground ?qsub_is ?qstat_is ?qdel_is host_name connection =
  {host_name; playground; connection;
   command_qsub = qsub_is; command_qstat = qstat_is; command_qdel = qdel_is}

let ssh  ?playground ?qsub_is ?qstat_is ?qdel_is ?name ?port ?user address =
  let host_name = match name with None -> address | Some n -> n in
  let connection = Host_SSH {address; port; user} in
  create ?playground ?qsub_is ?qstat_is ?qdel_is host_name connection

let to_json = Data_j.string_of_host
let of_json s =
  try `Ok (Data_j.host_of_string s) with e -> `Error (`Json_parsing e)

let name t = t.host_name
let to_string = name

let playground_exn t =
  match t.playground with
  | Some s -> s
  | None -> ksprintf  failwith "no playground for %s" t.host_name

let tmp_on_localhost = create ~playground:"/tmp" "Local:/tmp" Host_home

(** Generate a proper SSH command for the given host. *)
let do_ssh ~configuration ssh command =
  ["ssh"; Configuration.ssh_batch_option configuration]
  @ (match ssh.port with
    | Some p -> ["-p"; "port"]
    | None -> [])
  @ (match ssh.user with
    | None -> [ssh.address]
    | Some u -> [sprintf "%s@%s" u ssh.address])
  @ [command]

(** Generate an SCP command for the given host as destination. *)
let scp_push ~configuration ssh ~src ~dest =
  ["scp"; Configuration.ssh_batch_option configuration]
  @ (match ssh.port with
    | Some p -> ["-P"; "port"]
    | None -> [])
  @ src
  @ (match ssh.user with
    | None -> [sprintf "%s:%s" ssh.address dest]
    | Some u -> [sprintf "%s@%s:%s" u ssh.address dest])

(** Generate an SCP command for the given host as source. *)
let scp_pull ~configuration ssh ~src ~dest =
  ["scp"; Configuration.ssh_batch_option configuration]
  @ (match ssh.port with
    | Some p -> ["-P"; "port"]
    | None -> [])
  @ (List.map src ~f:(fun src_item ->
      match ssh.user with
      | None -> sprintf "%s:%s" ssh.address src_item
      | Some u -> sprintf "%s@%s:%s" u ssh.address src_item))
  @ [dest]

let run_commands ~configuration t cmds =
  begin match t.connection with
  | Host_home ->
    while_sequential cmds (fun cmd ->
        System.Shell.execute cmd
        >>= fun (out, err, status) ->
        begin match status with
        | `Exited 0 ->
          return (out, err)
        | other -> fail (`non_zero (out, err, other))
        end)
    >>< begin function
    | `Ok out_err_list ->
      let out, err =
        let outs, errs = List.split out_err_list in
        (String.concat ~sep:"" outs, String.concat ~sep:"" errs)
      in
      return (`Success (out, err))
    | `Error (`non_zero (out, err, e)) ->
      let msg =
        match e with
        |  (`Exited n) -> sprintf "exited:%d" n
        |  (`Signaled n) -> sprintf "signaled:%d" n
        |  (`Stopped n) -> sprintf "stopped:%d" n
      in
      return (`Failure (out, err, msg))
    | `Error  (`Shell (_, `Exn e)) ->
      let msg = sprintf "exception:%S" (Printexc.to_string e) in
      return (`Failure ("", "", msg))
    end
  | Host_SSH ssh ->
    let script = String.concat ~sep:" && " cmds in
    Process.exec (do_ssh ~configuration ssh script)
    >>= fun (out, err, status) ->
    begin match status with
    | `Exited 0 -> return (`Success (out, err))
    | code -> return (`Failure (out, err, Process.Exit_code.to_string code))
    end
  end

let with_playground ~configuration t path =
  match t.playground with
  | None -> fail (`host (`missing_playground t))
  | Some p ->
    let full = Filename.concat p path in
    begin match t.connection with
    | Host_home ->
      Process.succeed ["mkdir"; "-p"; full]
      >>= fun _ ->
      return full
    | Host_SSH ssh ->
      Process.succeed (do_ssh ~configuration ssh
                         (sprintf "mkdir -p %S" full))
      >>= fun _ ->
      return full
    end


let run_qsub ~configuration  t file =
  let cmd =
    sprintf "%s %S" (Option.value ~default:"qsub" t.command_qsub) in
  begin match t.connection with
  | Host_home -> run_commands ~configuration t [cmd file]
  | Host_SSH ssh ->
    with_playground ~configuration t "efmp_scripts/" >>= fun dest ->
    Process.succeed (scp_push ~configuration ssh ~src:[file] ~dest)
    >>= fun _ ->
    let remote_file = Filename.(concat dest (basename file)) in
    Process.succeed (do_ssh ~configuration ssh (cmd remote_file))
    >>= fun (out, err) ->
    return (`Success (out, err))
  end

let with_new_pbs_playground ~configuration t =
  let path =
    Filename.concat "efmp_monitored_pbs" (Unique_id.create ()) in
  with_playground ~configuration t path

let with_new_nohup_playground ~configuration t =
  let path =
    Filename.concat "efmp_monitored_nohup" (Unique_id.create ()) in
  with_playground ~configuration t path

let qsub_pbs_script ~configuration t pbs =
  let cmd =
    sprintf "%s %S" (Option.value ~default:"qsub" t.command_qsub) in
  with_new_pbs_playground ~configuration t
  >>= fun playground ->
  let actual_script = PBS_script.to_string ~playground pbs in
  let tmp = Filename.temp_file "efmp_monpbs_" ".pbs" in
  IO.write_file tmp ~content:actual_script
  >>= fun () ->
  begin match t.connection with
  | Host_home ->
    run_commands ~configuration t [cmd tmp]
    >>= fun res ->
    return (res, playground)
  | Host_SSH ssh ->
    let dest = Filename.concat playground "script.pbs" in
    Process.succeed (scp_push ~configuration ssh ~src:[tmp] ~dest)
    >>= fun _ ->
    Process.succeed (do_ssh ~configuration ssh (cmd dest))
    >>= fun (out, err) ->
    return (`Success (out, err), playground)
  end

let nohup_script ~configuration t nohup =
  let cmd script out err =
    sprintf "nohup setsid bash %s > '%s' 2> '%s' &" script out err in
  with_new_nohup_playground ~configuration t
  >>= fun playground ->
  let content = Nohup_script.to_string ~playground nohup in
  let out = Nohup_script.out_file ~playground in
  let err = Nohup_script.err_file ~playground in
  let tmp = Filename.temp_file "efmp_monnhp_" ".sh" in
  IO.write_file tmp ~content >>= fun () ->
  begin match t.connection with
  | Host_home ->
    run_commands ~configuration t [cmd tmp out err]
    >>= fun res ->
    return (res, playground)
  | Host_SSH ssh ->
    let dest = Filename.concat playground "script.sh" in
    Process.succeed (scp_push ~configuration ssh ~src:[tmp] ~dest)
    >>= fun _ ->
    Process.succeed (do_ssh ~configuration ssh (cmd dest out err))
    >>= fun (out, err) ->
    return (`Success (out, err), playground)
  end

let run_qstat ~configuration t job_id =
  let cmd =
    sprintf "%s -f1 %S" (Option.value ~default:"qstat" t.command_qstat) in
  begin match t.connection with
  | Host_home -> run_commands ~configuration t [cmd job_id]
  | Host_SSH ssh ->
    Process.exec (do_ssh ~configuration ssh (cmd job_id))
    >>= fun (out, err, code) ->
    begin match code with
    | `Exited 0 -> return (`Success (out, err))
    | other ->
      return (`Failure (out, err, Process.Exit_code.to_string other))
    end
  end

let run_qdel ~configuration t job_id =
  let cmd =
    sprintf "%s %S" (Option.value ~default:"qdel" t.command_qdel) in
  begin match t.connection with
  | Host_home -> run_commands ~configuration t [cmd job_id]
  | Host_SSH ssh ->
    Process.exec (do_ssh ~configuration ssh (cmd job_id))
    >>= fun (out, err, code) ->
    begin match code with
    | `Exited 0 -> return (`Success (out, err))
    | other ->
      return (`Failure (out, err, Process.Exit_code.to_string other))
    end
  end

let read_file_contents ~configuration t path =
  begin match t.connection  with
  | Host_home -> IO.read_file path
  | Host_SSH ssh ->
    let dest = Filename.temp_file "efmp_read_file" "bin" in
    Process.succeed (scp_pull ~configuration ssh ~src:[path] ~dest)
    >>= fun _ ->
    IO.read_file dest
  end

let read_monitored_script_log ~configuration t log_file_path =
  read_file_contents ~configuration t log_file_path
  >>< function
  | `Ok content ->
    let log = Monitored_script.parse_log content in
    return (`Log log)
  | `Error e ->
    return (`No_log)

let read_pbs_script_status ~configuration t ~playground =
  let log_file = PBS_script.log_file ~playground in
  read_monitored_script_log ~configuration t log_file

let read_nohup_script_status ~configuration t ~playground =
  let log_file = Monitored_script.log_file ~playground in
  read_monitored_script_log ~configuration t log_file

let kill_nohup_script ~configuration t ~playground =
  let pid_file = Monitored_script.pid_file ~playground in
  read_file_contents ~configuration t pid_file
  >>< function
  | `Ok content ->
    begin match Int.of_string (String.strip content) with
    | Some i ->
      let cmd = sprintf "kill -- -%d" i in
      begin match t.connection  with
      | Host_home -> run_commands ~configuration t [cmd]
      | Host_SSH ssh ->
        Process.exec (do_ssh ~configuration ssh (cmd))
        >>= fun (out, err, code) ->
        begin match code with
        | `Exited 0 -> return (`Success (out, err))
        | other ->
          return (`Failure (out, err, Process.Exit_code.to_string other))
        end
      end
    | None -> 
      return (`Failure ("", "", sprintf "Cannot parse PID file: %S" content))
    end
  | `Error e ->
    return (`Failure ("", "", "Cannot read PID file contents"))


(** Run `qstat` and interpret the result for any pbs id *)
let qstat ~configuration host pbs_id =
  run_qstat ~configuration host pbs_id
  >>= fun result ->
  begin match result with
  | `Success (out, err) ->
    begin of_result Result.(
        Pbs.Qstat.parse_qstat out
        >>= fun qstat ->
        Pbs.Qstat.get_status qstat
      )
      >>< function
      | `Ok s -> return s
      | `Error  (`qstat e) ->
        let msg =
          match e with
          | `job_state_not_found -> "no job state"
          | `no_header s -> sprintf "no header: %s" s
          | `unknown_status s -> sprintf "unknown-status: %s" s
          | `wrong_header_format s -> sprintf "header-format: %s" s
          | `wrong_lines _ -> sprintf "wrong-lines" in
        fail (`qstat_parsing msg)
    end
    >>= fun status ->
    begin match status with
    | `completed -> return `Job_completed
    | `queued -> return `Job_queued
    | `exiting
    | `held
    | `moved
    | `running
    | `suspended
    | `waiting -> return `Job_running
    end
  | `Failure (out, err, reason) ->
    (* failing when qstat fails should be optional,
             like a `--force` option *)
    return `Dont_know
  end

let file_exists host ~configuration ~path =
  begin match host.connection  with
  | Host_home -> 
    begin System.file_info path
      >>< function
      | `Ok `Absent | `Error _ -> return false
      | `Ok _ ->
        Debug.(s "file " % s path % s " exists" @ very_verbose);
        return true
    end
  | Host_SSH ssh ->
    let cmd = sprintf "test -e '%s'" path in
    Process.exec (do_ssh ~configuration ssh (cmd))
    >>= fun (out, err, code) ->
    begin match code with
    | `Exited 0 -> return true
    | other -> return false
    end
  end

