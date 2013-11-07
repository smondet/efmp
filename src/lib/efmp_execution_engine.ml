
open Efmp_internal_pervasives
module Configuration = Efmp_configuration
module Host = Efmp_host
open Efmp_meta_job

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
          let reason =
            sprintf "failed to start `make %s`: %s" make.make_name
              (List.map problems 
                 (fun (trgt, reason) ->
                    sprintf "%s: %s" Target.(to_string trgt) reason)
               |> String.concat ~sep:"; ") in
          let running = running_make_job make [] [reason] in
          add_completed ~running key ~status:(`Failure reason)
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
  let new_declared = ref [] in
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
          (* TODO: this can be highly optimized by starting some actions that
             have been freed by the ones succeeding. Right now we “wait” for
             all the current actions to finish before starting a new batch. *)
          let all_successes =
            List.for_all statuses ~f:(function (_, `Success) -> true | _ -> false) in
          begin match all_successes  with
          | false -> keep_going ()
          | true ->
            Make.start ~configuration make.running_make_todo
            >>= fun status ->
            begin match status with
            | `Failed_to_make problems ->
              let reason =
                sprintf "`make %s` failed: %s" 
                  make.running_make_todo.make_name
                  (List.map problems 
                     (fun (trgt, reason) ->
                        sprintf "%s: %s" Target.(to_string trgt) reason)
                   |> String.concat ~sep:"; ") in
              add_completed ~running ~key ~status:(`Failure reason)
            | `Should_start actions ->
              let keyed_actions =
                List.map actions (fun a -> (Unique_id.create (), a)) in
              let keys = List.map keyed_actions fst in
              log_change runtime "Make %s starts new actions:\n%s" key
                (List.map keyed_actions (fun (k,a) ->
                     sprintf "     * %s: %s" k (Todo.to_string a))
                 |> String.concat ~sep:"\n");
              new_declared := !new_declared @ keyed_actions;
              make.running_make_actions <- keys;
              keep_going ()
            | `Nothing_to_do _ ->
              log_change runtime "Nothing left to do for make %S %s"
                  make.running_make_todo.make_name key;
              add_completed ~running ~key ~status:`Success
            end
          end
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
  t.engine_declared <- !new_declared @ t.engine_declared;
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


let to_string
    ?(item_format="- $key:\n  $name\n  $status_with_reason\n$history_list")
    ?(all=true) t =
  let buf = Buffer.create 42 in
  let print fmt = ksprintf (Buffer.add_string buf) fmt in
  let section fmt = print ("# " ^^ fmt ^^ "\n\n") in
  let subsection fmt = print ("## " ^^ fmt ^^ "\n\n") in
  let par fmt =  print (fmt ^^ "\n\n") in
  let get_running_description =
    function
    | Running_empty h -> sprintf "Empty (host: %s)" (Host.to_string h)
    | Running_make m -> sprintf "Make: %s" m.running_make_todo.make_name
    | Running_pbs pbs -> 
      sprintf "PBS: %s as %s on %s"
        pbs.running_pbs_script.pbs_name 
        pbs.running_pbs_job_id 
        pbs.running_pbs_host.host_name
    | Running_nohup n ->
      sprintf "Nohup-setsid: %s in %s on %s"
        n.running_nohup_script.nohup_name 
        n.running_nohup_playground 
        n.running_nohup_host.host_name;
    | Running_get_output go ->
      sprintf "Get-output: %s on %s"
        go.running_get_output_name
        go.running_get_output_host.host_name
  in
  let get_history =
    function
    | Running_empty h -> []
    | Running_make m -> m.running_make_history
    | Running_pbs pbs -> pbs.running_pbs_history
    | Running_nohup n -> n.running_nohup_history
    | Running_get_output go -> go.running_get_output_history
  in
  let format_item ?running ?status key =
    let buf = Buffer.create 42 in
    Buffer.add_substitute buf (function
      | "n" -> "\n"
      | "key" -> key
      | "status" ->
        begin match status with
        | Some (`Success _) -> "SUCCESS"
        | Some (`Failure (_, _)) -> "FAILURE"
        | None -> ""
        end
      | "status_with_reason" ->
        begin match status with
        | Some (`Success _) -> "SUCCESS"
        | Some (`Failure (_, r)) -> sprintf "FAILURE: %s" r
        | None -> ""
        end
      | "name" -> 
        Option.value_map running ~default:"NO NAME" ~f:get_running_description 
      | "history_list" ->
        Option.value_map running ~default:[] ~f:get_history 
        |> List.map ~f:(sprintf "    * %s\n")
        |> String.concat ~sep:""
      | s -> sprintf "?? $%s ??" s) item_format;
    Buffer.contents buf
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
      print "%s" (format_item key ~running);
    );
  par "";
  if all then (
    subsection "Completed";
    List.iter t.engine_completed (fun (key, comp) ->
        let running =
          match comp.completed_status with
          | `Success r ->  Some r
          | `Failure (ro, r) ->  ro in
        print "%s" (format_item key ~status:comp.completed_status ?running);
        (* print "- `%s`:" key; *)
        (* begin match comp.completed_status with *)
        (* | `Success r -> print " **SUCCESS**\n"; print_running r; *)
        (* | `Failure (ro, r) -> *) 
        (*   print " **FAILURE**: %s\n" r; *) 
        (*   (match ro with None -> () | Some r -> print_running r); *)
      );
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


