
open Efmp_internal_pervasives

module Host = Efmp_host

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
module Wait_for = struct

  type t = wait_for
  let to_string t = t.wait_for_name

  let condition ~name ~and_start wait_for_condition =
    Do_wait_for {wait_for_name = name; wait_for_and_start = and_start;
                 wait_for_condition}

  let decide ~configuration t =
    Target.check ~configuration t.wait_for_condition
    >>= fun status ->
    begin match status with
    | `Done -> return (`Start t.wait_for_and_start)
    | `To_build -> return (`Wait)
    end

end
module Todo = struct
  type t = todo
  let to_string = function
  | Do_action a -> Action.to_string a
  | Do_make a -> Make.to_string a
  | Do_wait_for w -> Wait_for.to_string w
end

