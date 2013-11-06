
open Efmp_internal_pervasives
(* ************************************************************************** *)
(* Development *)
(* EDSL For Managing Processes *)

module Path = struct
  (** A Path is a [string] for now, but this will change in the future. *)

  type t = path

  let to_json = Data_j.string_of_path
  let of_json s =
    try `Ok (Data_j.path_of_string s) with e -> `Error (`Json_parsing e)
end

module Configuration = struct
  (** This module handles the “local” configuration, the configuration of the
      host from where the commands are run. *)

  type t = configuration

  let create ?(ssh_style=`Openssh) () = {ssh_style}

  let to_json = Data_j.string_of_configuration
  let of_json s =
    try `Ok (Data_j.configuration_of_string s)
    with e -> `Error (`Json_parsing e)

  (** Return a string which is the option to pass to the [ssh] command to
        avoid prompting for passwords. *)
  let ssh_batch_option t =
    match t.ssh_style with
    | `Openssh -> "-oBatchMode=yes"
    | `Dropbear -> "-s"

  let to_string t =
    sprintf "ssh-style: %s"
      (match t.ssh_style with
       | `Openssh -> "OpenSSH"
       | `Dropbear -> "Dropbear")
end

module Process = struct
  (** Manage external processes. *)

  module Exit_code = struct
    type t = [
      | `Exited of int
      | `Signaled of int
      | `Stopped of int
    ]
    let to_string = function
    | `Exited n ->   sprintf "exited:%d" n
    | `Signaled n -> sprintf "signaled:%d" n
    | `Stopped n ->  sprintf "stopped:%d" n

  end

  let exec ?(bin="") argl =
    wrap_deferred ~on_exn:(fun e -> `process (`exec (bin, argl), `exn e))
      Lwt.(fun () ->
          let command = (bin, Array.of_list argl) in
          let process = Lwt_process.open_process_full command in
          Lwt_list.map_p Lwt_io.read
            [process#stdout; process#stderr; ]
          >>= fun output_2list ->
          process#status >>= fun status ->
          return (status, output_2list))
    >>= fun (ret, output_2list) ->
    let code =
      match ret with
      | Lwt_unix.WEXITED n ->   (`Exited n)
      | Lwt_unix.WSIGNALED n -> (`Signaled n)
      | Lwt_unix.WSTOPPED n ->  (`Stopped n)
    in
    begin match output_2list with
    | [out; err] ->
      Debug.(s "Exec process: (" 
             % indent ~by:2 (s bin % s ", " % n
                             % string_list_map argl ~f:(sprintf "%S")) %n
             % s ")"
             % s " → "
             % a Exit_code.to_string code @ verbose;
             s "STDOUT:" %n %s out %n % s "STDERR:" %n %s err %n 
             @ very_verbose;
            );
      return (out, err, code)
    | _ -> assert false
    end

  let succeed ?(bin="") argl =
    exec ~bin argl
    >>= fun (out, err, status) ->
    let failure fmt =
      ksprintf (fun s -> fail (`process (`exec (bin, argl), `non_zero s)))
        fmt in
    begin match status with
    | `Exited 0 -> return  (out, err)
    | code -> failure "%s" (Exit_code.to_string code)
    end
end

module Monitored_script = struct
  (** The goal of this module is the create shell scripts from a high-level
      representation; the scripts are “monitored” in the sense that code is
      added to log every returned value or failure in a parsable [log] file.
  *)

  type t = monitored_script

  let create program = {program}

  let log_file ~playground =
    Filename.concat playground "log"

  let pid_file ~playground =
    Filename.concat playground "pid"

  let to_string ~playground t =
    let cmds = t.program in
    let log = log_file ~playground in
    let date = "date -u +'%F %T'" in
    let backquoted s = sprintf "`%s`" s in
    let to_log fmt =
      ksprintf (fun s -> sprintf "echo \"%s\" >> %s" s log) fmt in
    let tagged_log tag fmt =
      ksprintf (to_log "%s\t%s\t%s" tag (backquoted date)) fmt in
    let return_variable index = sprintf "return_of_%04d" index in
    let escape_for_echo cmd =
      let b = Buffer.create 42 in
      String.iter cmd (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9'
        | ' ' | '_' | '@' | '/' | '-' as c ->
          Buffer.add_char b c
        | other ->
          Buffer.add_string b "_");
      Buffer.contents b in
    let monitor index cmd =
      let ret_v = return_variable index in
      [ tagged_log "before-cmd" "CMD%04d\t%s" index (escape_for_echo cmd);
        sprintf "%s" cmd;
        sprintf "%s=$?" ret_v;
        tagged_log "after-cmd" "CMD%04d\treturned $%s" index ret_v;
        sprintf "if [ $%s -ne 0 ] ; then" ret_v;
        tagged_log "failure" "CMD%04d\treturned $%s" index ret_v;
        sprintf "exit $%s" ret_v;
        "fi";
      ] in
    let script =
      [ sprintf "mkdir -p %s" playground;
        sprintf "echo \"$$\" > %s" (pid_file playground);
        tagged_log "start" "" ]
      @ (List.mapi cmds monitor |> List.concat)
      @ [tagged_log "success" ""]
    in
    script |> String.concat ~sep:"\n"

  let parse_log log =
    let lines = String.split ~on:(`Character '\n') log in
    let table =
      List.filter_map lines ~f:(fun line ->
          let cells =
            String.split ~on:(`Character '\t') line
            |> List.map ~f:(String.strip)
            |> List.filter ~f:((<>) "")
          in
          match cells with
          | [] | [""] -> None
          | more ->
            let translated =
              match more with
              | "start" :: date :: [] -> `Start date
              | "before-cmd" :: date :: label :: cmd :: [] ->
                `Before (date, label, cmd)
              | "after-cmd" :: date :: label :: ret :: [] ->
                `After (date, label, ret)
              | "success" :: date :: [] -> `Success date
              | "failure" :: date :: label :: ret :: [] ->
                `Failure (date, label, ret)
              | other -> `Error other
            in
            Some translated)
    in
    table
end

module PBS_script = struct
  (** PBS scripts are here an enhanced and lazy version of the ones found in
      [ocaml-pbs], the library is then use to render them into files.  *)

  type t = pbs_script
  let create ~name
      ?(shell="/bin/bash")
      ?(walltime=`Hours 12.)
      ?(emailing=`Never)
      ?queue
      ?(nodes=1) ?(ppn=1) script =
    {
      pbs_script   = script  ;
      pbs_name     = name    ;
      pbs_shell    = shell   ;
      pbs_walltime = walltime;
      pbs_emailing = emailing;
      pbs_queue    = queue   ;
      pbs_nodes    = nodes   ;
      pbs_ppn      = ppn     ;
    }

  let log_file ~playground =
    Monitored_script.log_file ~playground

  let to_string ~playground t =
    let script = Monitored_script.to_string ~playground t.pbs_script in
    let stderr_path = Filename.concat playground "err" in
    let stdout_path = Filename.concat playground "out" in
    let pbs =
      Pbs.Script.raw
        ~name:t.pbs_name
        ~shell:t.pbs_shell
        ~walltime:t.pbs_walltime
        ~email_user:t.pbs_emailing
        ?queue:t.pbs_queue
        ~nodes:t.pbs_nodes
        ~ppn:t.pbs_ppn
        ~stderr_path
        ~stdout_path
        script in
    Pbs.Script.to_string pbs
end

module Nohup_script = struct
  (** This module is the parallel of [PBS_script] for monitored “nohup”-based
      scripts. *)

  type t = nohup_script
  let create ~name script =
    { nohup_name = name; nohup_script = script }

  let err_file ~playground = Filename.concat playground "err"
  let out_file ~playground = Filename.concat playground "out"

  let to_string ~playground t = 
    Monitored_script.to_string ~playground t.nohup_script

end
module Host = struct
  (** This module defines “hosts”: for now either the local machine, or a
      host accessible through SSH. *)
  type t = host


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
          | `exited 0 ->
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
          |  (`exited n) -> sprintf "exited:%d" n
          |  (`signaled n) -> sprintf "signaled:%d" n
          |  (`stopped n) -> sprintf "stopped:%d" n
        in
        return (`Failure (out, err, msg))
      | `Error  (`shell (_, `exn e)) ->
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
        | `Ok `absent | `Error _ -> return false
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

end

module Action = struct
  (** This module defines “actions”; a given thing to do on a given [Host.t].
  *)
  type t = action
  let create ?(host=Host.tmp_on_localhost) action =
    Do_action {host; action}

  let get_output ?host ~name cmds =
    create ?host (Action_get_output (name, cmds))

  let monitored_PBS ?host script = create ?host (Action_monitored_PBS script)
  let monitored_nohup ?host script =
    create ?host (Action_monitored_nohup script)

  let to_string t =
    sprintf "%s on %s"
      (match t.action with
       | Action_get_output (name, sl) -> sprintf "Get-output %s" name
       | Action_monitored_PBS t -> sprintf "Monitored-PBS %S" t.pbs_name
       | Action_monitored_nohup t -> sprintf "Monitored-Nohup %S" t.nohup_name
       | Action_empty -> "Empty")
      (Host.to_string t.host)

  let to_json = Data_j.string_of_action
  let of_json s =
    try `Ok (Data_j.action_of_string s) with e -> `Error (`Json_parsing e)


(** Start a simple action given its “Declared” initial state *)
  let start ~configuration action =
    begin match action.action with
    | Action_empty -> return `Success_empty
    | Action_get_output (name, cmds) ->
      Host.run_commands ~configuration action.host cmds
      >>= fun result ->
      begin match result with
      | `Success (out, err) -> 
        return (`Success_get_output (name, cmds, out, err))
      | `Failure (out, err, reason) ->
        return (`Failure_get_output (name, cmds, out, err, reason))
      end
    | Action_monitored_PBS (pbs) ->
      Host.qsub_pbs_script ~configuration action.host pbs
      >>= fun (result, playground) ->
      begin match result with
      | `Success (out, err) ->
        let pbs_id = String.strip out in
        return (`Start_pbs_job (pbs, pbs_id, playground))
      | `Failure (out, err, reason) ->
        return (`Fail_to_start_pbs_job (pbs, out, err, reason, playground))
      end
    | Action_monitored_nohup nohup ->
      Host.nohup_script ~configuration action.host nohup
      >>= fun (result, playground) ->
      begin match result with
      | `Success (out, err) ->
        return (`Start_nohup_job (nohup, playground))
      | `Failure (out, err, reason) ->
        return (`Fail_to_start_nohup_job (nohup, out, err, reason, playground))
      end
    end

end
module Target = struct

  let file_exists ?(host=Host.tmp_on_localhost) path =
    Target_file_exists {file_host = host; file_path = path}

  let check ~configuration =
    function
    | Target_file_exists {file_host; file_path} ->
      Host.file_exists ~configuration file_host ~path:file_path
      >>= fun exists ->
      if exists
      then return `Done
      else return `To_build

  let to_string = function
  | Target_file_exists {file_host; file_path} ->
    sprintf "@{%s::%s}" (Host.to_string file_host) file_path

end
module Make = struct

  type t = make

  let create ~name ~target make_rules =
    Do_make {make_name = name; make_final_target = target; make_rules}

  let to_string t = t.make_name

  let rule ?name ~target ~todo dependencies =
    {rule_name = Option.value name ~default:(Target.to_string target);
     rule_target = target; rule_todo = todo;
     rule_dependencies = dependencies}

  let rec start_making_target ~configuration t ~target =
    Target.check ~configuration target
    >>= fun status ->
    begin match status with
    | `Done -> return (`Nothing_to_do target)
    | `To_build -> 
      (* TODO: find rule that makes it and go recursively *)
      let rule_opt =
        List.find t.make_rules (fun rule -> rule.rule_target = target) in
      begin match rule_opt  with
      | Some rule ->
        while_sequential rule.rule_dependencies 
          (fun target -> start_making_target ~configuration t ~target)
        >>| List.filter ~f:(function `Nothing_to_do _ -> false | _ -> true)
        >>= fun starts ->
        begin match starts with
        | [] -> return (`Should_start [rule.rule_todo])
        | other ->
          let failed = 
            List.filter_map other 
              ~f:(function `Failed_to_make l -> Some l | _ -> None)
            |> List.concat in
          begin match failed with
          | [] ->
            let to_start =
              List.filter_map other 
                ~f:(function `Should_start l -> Some l | _ -> None)
              |> List.concat in
            return (`Should_start to_start)
          | more ->
            return (`Failed_to_make ((target, "Failed dependencies") :: more))
          end
        end
      | None -> return (`Failed_to_make [target, "No Rule"])
      end
    end

  let start ~configuration t =
    start_making_target ~configuration t ~target:t.make_final_target

end
module Todo = struct
  type t = todo
  let to_string = function
  | Do_action a -> Action.to_string a
  | Do_make a -> Make.to_string a
end

module Execution_engine = struct
  (** This module implements the basic functions that drive the actions
      (“[Action.t]”). *)


  type t = execution_engine

  let to_json t =
    Data_j.string_of_execution_engine t

  let of_json j =
    try
      Random.self_init ();
      `Ok (Data_j.execution_engine_of_string j)
    with
    | e -> `Error (`Json_parsing e)

  let logf t fmt =
    ksprintf (fun s ->
        t.engine_log <-
          (sprintf "%f" (Unix.gettimeofday ()), s) :: t.engine_log;
        return ()
      ) fmt

  let failure e = fail (`execution_engine e)

  let create ?ssh_style () =
    Random.self_init ();
    let configuration = Configuration.create ?ssh_style () in
    {engine_declared = [];
     engine_running = [];
     engine_completed = [];
     engine_log = [];
     engine_configuration = configuration;}

  let add_todo t ~todo =
    let key = Unique_id.create () in
    t.engine_declared <- (key, todo) :: t.engine_declared;
    return key

  let get_log t = t.engine_log

  type runtime = {
    mutable warnings: string list;
    mutable changes: string list;
  }
  let make_runtime () = {warnings = []; changes = []}
  let warn runtime fmt =
    ksprintf (fun s -> runtime.warnings <- s :: runtime.warnings) fmt
  let log_change runtime fmt =
    ksprintf (fun s -> runtime.changes <- s :: runtime.changes) fmt
  let merge_runtimes runtimes =
    { warnings = List.concat (List.map runtimes ~f:(fun r -> r.warnings));
      changes = List.concat (List.map runtimes ~f:(fun r -> r.changes)) }
  let runtimes_to_string runtimes =
    let runtime = merge_runtimes runtimes in
    sprintf "### Runtime Log\n\n%s%s"
      (match runtime.warnings with
       | [] -> "No warnings.\n"
       | l ->
         sprintf "Warnings:\n\n%s\n"
           (List.rev_map runtime.warnings ~f:(sprintf "- %s\n")
            |> String.concat ~sep:""))
      (match runtime.changes with
       | [] -> "No Changes.\n"
       | l -> 
         sprintf "Changes:\n\n%s\n"
           (List.rev_map runtime.changes ~f:(sprintf "- %s\n")
            |> String.concat ~sep:""))

  let pbs_history state fmt =
    ksprintf (fun s ->
        state.running_pbs_history <-
          sprintf "[%s] %s" Time.(now () |> to_filename) s
          :: state.running_pbs_history;
      ) fmt
  let nohup_history state fmt =
    ksprintf (fun s ->
        state.running_nohup_history <-
          sprintf "[%s] %s" Time.(now () |> to_filename) s
          :: state.running_nohup_history;
      ) fmt

  let start_all_declared t ~runtime =
    let configuration = t.engine_configuration in
    let new_completed = ref [] in
    let add_completed ?stdout ?stderr key ~status ~running =
      let completed_status =
        match status with
        | `Success -> 
          log_change runtime "%s SUCCEEDED" key;
          `Success running
        | `Failure r ->
          log_change runtime "%s FAILED: %s" key r;
          `Failure (Some running, r) in
      new_completed := (key,
                        {completed_status;
                         completed_stdout = stdout;
                         completed_stderr = stderr; }) :: !new_completed;
      return []
    in
    let running_get_output host name cmds =
      Running_get_output {
        running_get_output_host = host;
        running_get_output_name = name;
        running_get_output_commands = cmds;
        running_get_output_history = [];
      }  in
    let running_nohup_job host script playground =
      Running_nohup  {
        running_nohup_host = host;
        running_nohup_script = script;
        running_nohup_pid = None;
        running_nohup_playground = playground;
        running_nohup_history = []; }
    in
    let running_pbs_job host script job_id playground =
      Running_pbs  {
        running_pbs_host = host;
        running_pbs_script = script;
        running_pbs_job_id = job_id;
        running_pbs_playground = playground;
        running_pbs_history = []; }
    in
    let running_make_job make actions history =
      Running_make {
        running_make_todo = make;
        running_make_actions = actions;
        running_make_history = history;
      } in
    let new_running = ref [] in
    while_sequential t.engine_declared (fun (key, todo) ->
        Debug.(s "start_all_declared: " % s key @ very_verbose);
        match todo with
        | Do_action action ->
          Action.start ~configuration action
          >>= fun status ->
          begin match status with
          | `Success_empty ->
            let running = Running_empty action.host in
            log_change runtime "%s Started and succeeded" key;
            add_completed key ~status:`Success ~running
          | `Success_get_output (name, cmds, stdout, stderr) ->
            log_change runtime "%s Started and succeeded" key;
            let running = running_get_output action.host name cmds in
            add_completed ~running ~stdout ~stderr key ~status:`Success
          | `Failure_get_output  (name, cmds, stdout, stderr, reason) ->
            log_change runtime "%s Failed to start" key;
            let running = running_get_output action.host name cmds in
            add_completed ~running ~stdout ~stderr key ~status:(`Failure reason)
          | `Fail_to_start_nohup_job (nohup, stdout, stderr, reason, playground) ->
            log_change runtime "%s Failed to start (nohup)" key;
            let running = running_nohup_job action.host nohup playground in
            add_completed ~running ~stdout ~stderr key ~status:(`Failure reason)
          | `Fail_to_start_pbs_job (pbs, stdout, stderr, reason, playground) ->
            log_change runtime "%s Failed to start (PBS)" key;
            let running = running_pbs_job action.host pbs "" playground in
            add_completed ~running ~stdout ~stderr key ~status:(`Failure reason)
          | `Start_nohup_job (nohup, playground) ->
            let run = running_nohup_job action.host nohup playground in
            log_change runtime "%s Started (nohup)" key;
            new_running := (key, run) :: !new_running;
            return []
          | `Start_pbs_job (pbs, pbs_id, playground) ->
            log_change runtime "%s Started (PBS)" key;
            let run = running_pbs_job action.host pbs pbs_id playground in
            new_running := (key, run) :: !new_running;
            return []
          end
        | Do_make make ->
          Make.start ~configuration make
          >>= fun status ->
          begin match status with
          | `Failed_to_make problems ->
            warn runtime "failed to start `make %s`: %s" make.make_name
              (List.map problems 
                 (fun (trgt, reason) ->
                    sprintf "%s: %s" Target.(to_string trgt) reason)
               |> String.concat ~sep:"; ");
            return [key, Do_make make]
          | `Should_start actions ->
            let keyed_actions =
              List.map actions (fun a -> (Unique_id.create (), a)) in
            let running = 
              running_make_job make (List.map keyed_actions fst) ["Started"]
            in
            log_change runtime "Make %s starts with actions:\n%s" key
              (List.map keyed_actions (fun (k,a) ->
                   sprintf "     * %s: %s" k (Todo.to_string a))
               |> String.concat ~sep:"\n");
            new_running := (key, running) :: !new_running;
            return keyed_actions
          | `Nothing_to_do _ ->
            (* If start replies `Nothing_to_do it must be the final target *)
            log_change runtime "Nothing to do for make %S %s"
              make.make_name key;
            let running = running_make_job make [] ["Nothing_to_do"] in
            add_completed ~running key ~status:`Success
          end
      )
    >>| List.concat 
    >>= fun new_declared ->
    t.engine_declared <- new_declared;
    t.engine_running <- !new_running @ t.engine_running;
    t.engine_completed <- !new_completed @ t.engine_completed;
    return ()

  let update_from_monitored_script_status ~runtime script_status =
    let still_running fmt = ksprintf (fun s -> return (`Running s)) fmt in
    begin match script_status with
    | `Log log ->
      begin match List.rev log with
      | `Start s :: _ ->
        still_running "started %s" s
      | `After (a,b,c) :: _
      | `Before (a,b,c) :: _ ->
        still_running "at command %s %s %s" a b c
      | [] ->
        warn runtime "Got empty log";
        still_running "empty log"
      | `Error l :: _ ->
        let log = String.concat ~sep:", " l in
        warn runtime "Got erroneous log: %s" log;
        still_running "erroneous log: %s" log
      | `Success date :: _ ->
        return (`Succeeded date)
      | `Failure (date, label, ret) :: _ ->
        let reason = sprintf "%s %s" label ret in
        return (`Failed (date, reason))
      end
    | `No_log ->
      warn runtime "Found no log";
      still_running "no log"
    end

  let update_all_running t ~runtime =
    let configuration = t.engine_configuration in
    let new_completed = ref [] in
    let add_completed ~key ~status ~running =
      let completed_status =
        match status with
        | `Success -> 
          log_change runtime "%s SUCCEEDED" key;
          `Success running
        | `Failure r ->
          log_change runtime "%s FAILED: %s" key r;
          `Failure (Some running, r) in
      new_completed := (key,
                        {completed_status;
                         completed_stdout = None;
                         completed_stderr = None; }) :: !new_completed;
      return []
    in
    let get_status t ~key =
      try ignore (List.assoc key t.engine_declared); `Declared
      with _ ->
        begin try ignore (List.assoc key t.engine_running); `Running
        with _ ->
          begin try 
            let completed = List.assoc key t.engine_completed in
            begin match completed.completed_status with
            | `Success _ -> `Success
            | `Failure _ -> `Failure
            end
          with _ -> `Dont_know
          end
        end
    in
    while_sequential t.engine_running (fun (key, running) ->
        let keep_going () = return [key, running] in
        Debug.(s "update_all_running: " % s key @ very_verbose);
        begin match running with
        | Running_empty _ ->
          warn runtime "there should not be a `Running_empty` actually running";
          keep_going ()
        | Running_get_output _ ->
          warn runtime "there should not be a `Running_get_output` actually running";
          keep_going ()
        | Running_make make ->
          (* go though actions,
             any failed → fail completely, kill the other ones (?)n
             for any completed action -> reevaluate target? of that action only!
          *)
          let statuses =
            List.map make.running_make_actions 
              (fun key -> (key, get_status t ~key))
          in
          let failures =
            List.filter_map statuses ~f:(function
              | (key, `Failure) | (key, `Dont_know) -> Some key
              | _ -> None)
          in
          begin match failures with
          | [] ->
            warn runtime "%s Running `Make` jobs is NOT IMPLEMENTED" 
              make.running_make_todo.make_name;
            (* TODO *)
            keep_going ()
          | some ->
            Debug.(s "Actions " % string_list some % s " are killing Make "
                   % s make.running_make_todo.make_name
                   % in_parens (s key) @ very_verbose);
            (* TODO: should we kill the other ones? *)
            add_completed ~key ~status:(`Failure "Actions died") ~running
          end
        | Running_pbs rpbs ->
          Host.qstat ~configuration 
            rpbs.running_pbs_host rpbs.running_pbs_job_id
          >>= fun qstat_result ->
          begin match qstat_result with
          | `Job_queued ->
            pbs_history rpbs "Job_queued";
            keep_going ()
          | `Job_running ->
            pbs_history rpbs "Job_running";
            keep_going ()
          | `Job_completed | `Dont_know ->
            let host = rpbs.running_pbs_host in
            let playground = rpbs.running_pbs_playground in
            Host.read_pbs_script_status ~configuration host ~playground
            >>= fun script_status ->
            update_from_monitored_script_status ~runtime script_status
            >>= fun update ->
            begin match update with
            | `Running log ->
              pbs_history rpbs "Job_running: %s" log;
              keep_going ()
            | `Succeeded log  ->
              pbs_history rpbs "Job_succeeded: %s" log;
              add_completed  ~key ~status:`Success ~running
            | `Failed (date, reason) ->
              pbs_history rpbs "Job_failed: %s (%s)" reason date;
              add_completed ~key ~status:(`Failure reason) ~running
            end
          end
        | Running_nohup rnohup ->
          let host = rnohup.running_nohup_host in
          let playground = rnohup.running_nohup_playground in
          Host.read_nohup_script_status ~configuration host ~playground
          >>= fun script_status ->
          update_from_monitored_script_status ~runtime script_status
          >>= fun update ->
          begin match update with
          | `Running log ->
            nohup_history rnohup "Job_running: %s" log;
            keep_going ()
          | `Succeeded log  ->
            nohup_history rnohup "Job_succeeded: %s" log;
            add_completed ~key ~status:`Success ~running
          | `Failed (date, reason) ->
            nohup_history rnohup "Job_failed: %s (%s)" reason date;
            add_completed ~key ~status:(`Failure reason) ~running
          end
        end)
    >>| List.concat
    >>= fun new_running ->
    t.engine_running <- new_running;
    t.engine_completed <- !new_completed @ t.engine_completed;
    return ()


(** Loop on all jobs and try to update their status *)
  let wake_up t =
    let runtime = make_runtime () in
    start_all_declared t ~runtime
    >>= fun () ->
    update_all_running t ~runtime
    >>= fun () ->
    return (runtime)

  let kill t ~key =
    let configuration = t.engine_configuration in
    let runtime = make_runtime () in
    let new_completed = ref [] in
    let add_completed ?running key ~reason =
      let completed_status =
        log_change runtime "%s Was KILLED: %s" key reason;
        `Failure (running, reason) in
      new_completed := (key,
                        {completed_status;
                         completed_stdout = None;
                         completed_stderr = None; }) :: !new_completed;
      return None
    in
    while_sequential t.engine_declared (fun (k, declaration) ->
        if k = key
        then add_completed key ~reason:"Killed by user before starting"
        else return (Some (k, declaration)))
    >>| List.filter_map ~f:(fun x -> x)
    >>= fun new_declared ->
    while_sequential t.engine_running (function
      | (k, running) when k = key ->
        let kill fmt =
          ksprintf (fun reason -> add_completed key ~reason ~running) fmt in
        let still_running () = return (Some (k, running)) in
        begin match running with
        | Running_empty _ ->
          warn runtime "there should not be a `Running_empty` actually running";
          kill "Killed by user (empty)"
        | Running_get_output _ ->
          warn runtime "there should not be a `Running_get_output` actually running";
          kill "Killed by user (get_output)"
        | Running_make _ ->
          warn runtime "Killing `Make` jobs: NOT IMPLEMENTED";
          still_running ()
        | Running_pbs rpbs ->
          Host.qstat ~configuration 
            rpbs.running_pbs_host rpbs.running_pbs_job_id
          >>= fun qstat_result ->
          begin match qstat_result with
          | `Job_queued | `Job_running | `Dont_know ->
            Host.run_qdel ~configuration
              rpbs.running_pbs_host rpbs.running_pbs_job_id
            >>= fun result ->
            begin match result  with
            | `Success _ ->
              kill "Killed by user (qdel)"
            | `Failure _ ->
              warn runtime "qdel failed";
              still_running ()
            end
          | `Job_completed ->
            kill "Killed by user (qstat: completed)"
          end
        | Running_nohup rnohup ->
          Host.kill_nohup_script ~configuration
            rnohup.running_nohup_host
            ~playground:rnohup.running_nohup_playground
          >>= fun result ->
          begin match result  with
          | `Success _ ->
            kill "Killed by user (kill process group)"
          | `Failure (_, _, s) ->
            warn runtime  "kill_nohup_script FAILURE: %s" s;
            still_running ()
          end
        end
      | (k, running) ->
        return (Some (k, running)))
    >>| List.filter_map ~f:(fun x -> x)
    >>= fun new_running ->
    t.engine_declared <- new_declared;
    t.engine_running <- new_running;
    t.engine_completed <- !new_completed @ t.engine_completed;
    return (runtime)


  let to_string ?(all=true) t =
    let buf = Buffer.create 42 in
    let print fmt = ksprintf (Buffer.add_string buf) fmt in
    let section fmt = print ("# " ^^ fmt ^^ "\n\n") in
    let subsection fmt = print ("## " ^^ fmt ^^ "\n\n") in
    let par fmt =  print (fmt ^^ "\n\n") in
    let print_running =
      function
      | Running_empty h -> print "Empty (host: %s)\n" (Host.to_string h)
      | Running_make m -> print "Make: %s\n" m.running_make_todo.make_name
      | Running_pbs pbs -> 
        print "PBS: %s as %s on %s\n"
          pbs.running_pbs_script.pbs_name 
          pbs.running_pbs_job_id 
          pbs.running_pbs_host.host_name;
        List.iter pbs.running_pbs_history (print "    * %s\n")
      | Running_nohup n ->
        print "Nohup-setsid: %s in %s on %s\n"
          n.running_nohup_script.nohup_name 
          n.running_nohup_playground 
          n.running_nohup_host.host_name;
        List.iter n.running_nohup_history (print "    * %s\n")
      | Running_get_output go ->
        print "Get-output: %s on %s\n"
          go.running_get_output_name
          go.running_get_output_host.host_name;
        List.iter go.running_get_output_history (print "    * %s\n")
    in
    section "Configuration";
    par "%s" (Configuration.to_string t.engine_configuration);
    section "State";
    subsection "Declared To Do";
    List.iter t.engine_declared (fun (key, todo) ->
        print "- `%s`:\n" key;
        match todo with
        | Do_make make -> print "Make %s\n" make.make_name
        | Do_action action -> print "%s\n" (Action.to_string action));
    par "";
    subsection "Running";
    List.iter t.engine_running (fun (key, running) ->
        print "- `%s`:\n" key;
        print_running running;
      );
    par "";
    if all then (
      subsection "Completed";
      List.iter t.engine_completed (fun (key, comp) ->
          print "- `%s`:" key;
          begin match comp.completed_status with
          | `Success r -> print " **SUCCESS**\n"; print_running r;
          | `Failure (ro, r) -> 
            print " **FAILURE**: %s\n" r; 
            (match ro with None -> () | Some r -> print_running r);
          end);
      par "";
    );
    section "Log%s" (if not all then " (truncated)" else "");
    let rec loop n = 
      function
      | l when n = 0 -> ()
      | (d , l) :: t -> print "- [%s] %s\n" d l; loop (n - 1) t
      | [] -> () in
    loop 8 t.engine_log;
    par "";
    Buffer.contents buf

end

(** High-level Manipulations of [Execution_engine.t] *)
module Command = struct
  (** This module implements command-line oriented higher-level functions to
      manipulate the [Execution_engine.t].  *)

  let with_engine ~persist_with ~f =
    begin
      IO.read_file persist_with
      >>< function
      | `Ok content ->
        begin
          of_result (Execution_engine.of_json content)
          >>= fun engine ->
          f engine
          >>= fun () ->
          IO.write_file persist_with ~content:(Execution_engine.to_json engine)
        end
      | `Error (`read_file_error (_, _)) ->
        fail (`failure (sprintf "Cannot open the persistence file: %s, maybe the \
                                 instance is not initialized?" persist_with))
    end


  let say fmt =
    ksprintf (fun s -> printf "EFMP: %s\n%!" s; return  ()) fmt

  let display fmt =
    ksprintf (fun s -> printf "%s\n%!" s; return ()) fmt

  let display_info ~persist_with ~all () =
    with_engine ~persist_with ~f:(fun engine ->
        display "%s" (Execution_engine.to_string ~all engine)
      )

  let display_log ~persist_with () =
    with_engine ~persist_with ~f:(fun engine ->
        while_sequential (Execution_engine.get_log engine) (fun (a, b) ->
            display "[%s]\n    %s" (Time.to_filename (float_of_string a)) b)
        >>= fun _ ->
        return ())

  let do_init ~persist_with () =
    let engine = Execution_engine.create () in
    let content = Execution_engine.to_json engine in
    System.ensure_directory_path (Filename.dirname persist_with)
    >>= fun () ->
    IO.write_file persist_with ~content
    >>= fun () ->
    say "Done: wrote `%s`." persist_with

  let do_wake_up ~persist_with () =
    with_engine ~persist_with ~f:(fun engine ->
        Execution_engine.wake_up engine
        >>= fun info ->
        say "Wake up results:\n%s"
          (Execution_engine.runtimes_to_string [info]))

  (** [with_cbreak f] calls with the terminal in “get key” mode. 
         It comes from
         http://pleac.sourceforge.net/pleac_ocaml/userinterfaces.html
  *)
  let with_cbreak (f: unit -> (_, _) t) =
    let open Lwt_unix in
    Lwt.(tcgetattr stdin
         >>= fun term_init ->
         let term_cbreak = { term_init with c_icanon = false } in
         tcsetattr stdin TCSANOW term_cbreak
         >>= fun () ->
         catch f (fun exn -> return (`Error (`failure "with_cbreak")))
         >>= fun res ->
         tcsetattr stdin TCSADRAIN term_init
         >>= fun () ->
         return res)

  let get_key () =
    with_cbreak (fun () ->
        wrap_deferred (fun () -> Lwt_io.read_char Lwt_io.stdin)
          ~on_exn:(fun e -> (`failure "get_key")))

  let do_murder ~persist_with ~keys () =
    with_engine ~persist_with ~f:(fun engine ->
        let kill_at_key key =
          say "Killing %s\n" key
          >>= fun () ->
          Execution_engine.kill engine ~key in
        match keys with
        | [] -> 
          say "Interactive mass murder of jobs:"
          >>= fun () ->
          let all_keys =
            List.map engine.engine_declared fst
            @  List.map engine.engine_running fst in
          while_sequential all_keys (fun key ->
              say "Kill %s? [y,N]" key
              >>= fun () ->
              get_key ()
              >>= fun c ->
              begin match c with
              | 'y' | 'Y' ->
                kill_at_key key
                >>= fun info ->
                say "killing %s\n%s" key
                  (Execution_engine.runtimes_to_string [info])
              | c -> say "Letting %s survive" key
              end
            )
          >>= fun _ ->
          say "Done."
        | more ->
          while_sequential more kill_at_key
          >>= fun runtimes ->
          say "Done:\n%s" (Execution_engine.runtimes_to_string runtimes))


  let add_todos ~persist_with alist =
    with_engine ~persist_with ~f:(fun engine ->
        while_sequential alist (fun a ->
            Execution_engine.add_todo engine a
            >>= fun id ->
            say "Added new TODO-item as %s" id)
        >>= fun _ ->
        say "Done.")
end

module Command_line = struct
  (** This module gives a user interface to the functions in the [Command]
      module using the [cmdliner] library. *)

  let default_persistence_file = 
    ref (
      Filename.concat (Sys.getenv "HOME") ".efmp/persist.json"
    )

  let cmdliner_main make_actions_term =
    let open Cmdliner in
    let open Command in
    let version = "0.0.0" in
    let persistence_file_arg =
      let docv = "FILE" in
      let doc = "Use $(docv) as persistence." in
      Arg.(value & opt string !default_persistence_file
           & info ["P"; "persistence-file"] ~docv ~doc)
    in
    let list_of_arguments ~doc =
      Arg.(value & pos_all string [] & info [] ~docv:"ARGS" ~doc)
    in
    let info_cmd =
      let doc = "Get info about this instance." in
      let man = [] in
      Term.(
        pure (fun persist_with all -> display_info ~all ~persist_with)
        $ persistence_file_arg
        $ Arg.(value & flag & info ["A"; "all"] 
                 ~doc:"Display all processes even the completed ones.")
      ),
      Term.info "info" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man
    in
    let log_cmd =
      let doc = "Display the logs of this instance." in
      let man = [] in
      (Term.((
           pure (fun persist_with -> display_log ~persist_with)
           $ persistence_file_arg
         )),
       Term.info "log" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man)
    in
    let init_cmd =
      let doc = "Initialize the efmp instance." in
      let man = [] in
      (Term.((
           pure (fun persist_with -> do_init ~persist_with)
           $ persistence_file_arg
         )),
       Term.info "init" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man)
    in
    let wake_up_cmd =
      let doc = "Wake-up the efmp instance to update itself." in
      let man = [] in
      (Term.((
           pure (fun persist_with -> do_wake_up ~persist_with)
           $ persistence_file_arg
         )),
       Term.info "wake-up" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man)
    in
    let kill_cmd =
      let doc = "Wake-up the efmp instance to update itself." in
      let man = [] in
      (Term.((
           pure (fun persist_with keys -> do_murder ~persist_with ~keys)
           $ persistence_file_arg
           $ list_of_arguments ~doc:"process-keys to kill"
         )),
       Term.info "kill" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man)
    in
    let start_cmd =
      let doc = "Add an action and start it (wake-up)." in
      let man = [] in
      (Term.((
           pure (fun persist_with dry_run todos () ->
               if dry_run
               then begin
                 say "Dry-run, Not-doing: ["
                 >>= fun () ->
                 while_sequential todos (function
                   | Do_action a ->
                     say "    %s" (Action.to_string a)
                   | Do_make m ->
                     say "    %s" (Make.to_string m))
                 >>= fun _ ->
                 say "]"
               end else begin
                 match todos with
                 | [] ->
                   say "start: no actions to start :-("
                 | todos ->
                   add_todos ~persist_with todos
               end)
           $ persistence_file_arg
           $ Arg.(value & flag & info ["n"; "dry-run"]
                    ~doc:"Only display the TODO-items that would have been \
                          activated.")
           $ make_actions_term
         )),
       Term.info "start" ~version ~sdocs:"COMMON OPTIONS" ~doc ~man)
    in
    let default_cmd =
      let some_opts = Term.(pure (fun () -> ()) $ pure ()) in
      let doc = "EFMP administration tool." in
      let man = [] (* TODO *) in
      (Term.(ret (pure (fun () -> `Help (`Plain, None)) $ some_opts)),
       Term.info Sys.argv.(0) ~version ~sdocs:"COMMON OPTIONS" ~doc ~man)
    in
    let cmds =
      [info_cmd; init_cmd; wake_up_cmd; start_cmd; log_cmd; kill_cmd] in
    match Term.eval_choice default_cmd cmds with
    | `Ok f -> f
    | `Error _ -> exit 1
    | _ -> exit 0

  let error_to_string =
    let exn = Printexc.to_string in
    function
    | `failure s -> sprintf "Failure: %s" s
    | `Json_parsing e -> sprintf "Parsing JSON: %s" (exn e)
    | `read_file_error (s, e) -> sprintf "error while reading: %S: %s" s (exn e)
    | `write_file_error (s, e) -> sprintf "error while writing: %S: %s" s (exn e)
    | `process (`exec (bin, cmd), e) ->
      sprintf "Error while executing: (%S, [%s]): %s"
        bin (List.map cmd ~f:(sprintf "%S") |> String.concat ~sep:"; ")
        (match e with
         | `exn e -> (exn e)
         | `non_zero s -> sprintf "non-zero status: %s" s)
    | (`host (`missing_playground t)) ->
      sprintf "Host %s is missing its playground." (Host.to_string t)
    | `qstat_parsing s ->
      sprintf "Error while parsing QStat output: %s" s
    | `shell (cmd, _) ->
      (* [> `exited of int | `exn of exn | `signaled of int | `stopped of int ] *)
      sprintf "Shell command %S failed" cmd
    | `system (what, wrong) ->
      sprintf "System error while %s: %s"
        begin match what with
        | `make_directory s -> sprintf "making directory %S" s
        end
        begin match wrong with
        | `exn e -> sprintf "Exception: %s" (exn e)
        | `wrong_access_rights i -> sprintf "Wrong access rights: %o" i
        end

  let run_main make_actions_term =
    match Lwt_main.run (cmdliner_main make_actions_term ()) with
    | `Ok () -> exit 0
    | `Error e ->
      eprintf "ERROR: \n%s\n%!" (error_to_string e);
      exit 1
end



