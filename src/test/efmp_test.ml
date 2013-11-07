
#use "topfind"
#thread
#require "nonstd,sosa,pvem,atdgen,cmdliner,pvem_lwt_unix,pbs"
#directory "_build/src/lib"
#load "efmp.cma"


open Nonstd
module String = struct
  include Sosa.Native_string
end
open Efmp

(* That's how is should look *)

let say fmt = ksprintf (eprintf "EFMP-test: %s\n%!") fmt

let failwithf fmt = ksprintf failwith fmt

(* Create a host out an ssh hostname, usually something in .ssh/config *)
let ssh_host name =
  Host.ssh ~playground:"/tmp/efmp_playground/" name

(* A command that always fails *)
let fail_cmd = "x=`exit 42`"

let status_in_name status name =
  sprintf "%s-%s" name (if status then "SUCCEED" else "FAIL")

let test_get_output ~succeed ~host =
  let name = status_in_name succeed "get_output" in
  Action.get_output ~name ~host (if succeed then ["ls"] else [fail_cmd])

let test_nohup_script ~succeed ~host =
  let name = status_in_name succeed "nohup" in
  Action.monitored_nohup ~host
    (Nohup_script.create ~name 
       (Monitored_script.create [
           "date -R";
           if succeed then "ls" else fail_cmd
         ]))

let always_existing_file host = Target.(file_exists ~host "/etc/passwd")
let never_existing_file host = Target.(file_exists ~host "/zzzzzzzzzzzzzzz")

let make_nothing ~succeed host =
  let name =
    sprintf "make-mothing-%s" (Host.name host) |> status_in_name succeed  in
  let target =
    if succeed
    then (always_existing_file host)
    else (never_existing_file host)
  in
  Make.create ~name ~target []

let new_host_temp_file host =
  Filename.concat (Host.playground_exn host) 
    (sprintf "test_files/%s" (Unique_id.create ()))

let make_one_file_target_and_todo ~succeed host =
  let file = new_host_temp_file host in
  let target = Target.(file_exists ~host file) in
  let todo = 
    let name =
      sprintf "touch-test-file-%s" (Host.name host) |> status_in_name succeed in
    Action.get_output ~host ~name [
      sprintf "mkdir -p `dirname %s`" file;
      if succeed
      then sprintf "touch '%s'" file
      else fail_cmd
    ]
  in
  (target, todo, file)

let make_one_file ~succeed host =
  let (target, todo, _) = make_one_file_target_and_todo ~succeed host  in
  let name =
    sprintf "make-test-file-%s" (Host.name host) |> status_in_name succeed in
  Make.(create ~name ~target [
      rule ~target ~todo [];
    ])

let make_one_file_and_copy_it ~succeed_step_one ~succeed_step_two host =
  let (target_one_file, todo_one_file, src_of_copy) =
    make_one_file_target_and_todo ~succeed:succeed_step_one host  in
  let dst_of_copy = new_host_temp_file host in
  let target = Target.(file_exists ~host dst_of_copy) in
  let todo =
    let name =
      sprintf "copy-test-file-%s" (Host.name host)
      |> status_in_name succeed_step_two
    in
    Action.get_output ~host ~name [
      sprintf "mkdir -p `dirname %s`" dst_of_copy;
      if succeed_step_two
      then sprintf "cp '%s' '%s'" src_of_copy dst_of_copy
      else fail_cmd
    ] in
  let name =
    sprintf "make-and-copy-test-file-%s" (Host.name host)
    |> status_in_name succeed_step_one
    |> status_in_name succeed_step_two
  in
  Make.(create ~target ~name [
      rule ~target ~todo [target_one_file];
      rule ~target:target_one_file ~todo:todo_one_file [];
    ])


let () =
  let make_actions_term =
    let make_actions hosts arguments =
      say "Using hosts: [%s]" (String.concat ~sep:", " hosts);
      let actual_hosts = 
        if hosts = []
        then [Host.tmp_on_localhost] 
        else (List.map hosts ssh_host) in
      match arguments with
      | ["test"] -> 
        List.map actual_hosts (fun host ->
            List.map [true;false] (fun succeed ->
                [test_get_output ~succeed ~host;
                 test_nohup_script ~succeed ~host;
                 make_nothing ~succeed host;
                 make_one_file ~succeed host;
                ])
            @ begin
              List.map [true; false] (fun succeed_step_one ->
                  List.map [true; false] (fun succeed_step_two ->
                      [make_one_file_and_copy_it host
                         ~succeed_step_one ~succeed_step_two]
                    )) |> List.concat
            end)
        |> List.concat 
        |> List.concat 
      | l ->
        say "Don't know what to do with [%s]" (String.concat ~sep:", " l);
        []
    in
    let open Cmdliner in
    let hosts_option =
      let docv = "SSH-HOST" in
      let doc = "Use a given $(docv) as host for the test" in
      Arg.(value & opt (list string)  []
           & info ["S"; "ssh"] ~docv ~doc)
    in
    let all_args =
      let doc = " TODO: write some doc here " in
      Arg.(value & pos_all string [] & info [] ~docv:"SPECIFICATION" ~doc)
    in
    Term.(pure make_actions
          $ hosts_option $ all_args)
  in
  Command_line.run_main make_actions_term

