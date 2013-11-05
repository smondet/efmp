
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


let failwithf fmt = ksprintf failwith fmt

let () =
  let make_actions_term =
    let make_actions say_something arguments =
      eprintf "### %s ###\n%!" say_something;
      match arguments with
      | _ -> []
    in
    let open Cmdliner in
    let say_something =
      let docv = "STRING" in
      let doc = "Say '$(docv)'." in
      Arg.(value & opt string "NOTHING TO SAY"
           & info ["S"; "say"] ~docv ~doc)
    in
    let all_args =
      let doc = " TODO: write some doc here " in
      Arg.(value & pos_all string [] & info [] ~docv:"SPECIFICATION" ~doc)
    in
    Term.(pure make_actions
          $ say_something $ all_args)
  in
  Command_line.run_main make_actions_term

