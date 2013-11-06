
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

let ssh_host name =
  Host.ssh ~playground:"/tmp/efmp_playground/" name

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
                 test_nohup_script ~succeed ~host;]))
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

