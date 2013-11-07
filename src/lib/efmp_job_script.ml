
open Efmp_internal_pervasives

module Monitored_script = struct

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

  let to_string t ~playground =
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

  type t = nohup_script
  let create ~name script =
    { nohup_name = name; nohup_script = script }

  let err_file ~playground = Filename.concat playground "err"
  let out_file ~playground = Filename.concat playground "out"

  let to_string t ~playground = 
    Monitored_script.to_string ~playground t.nohup_script

end

