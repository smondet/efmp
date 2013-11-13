(** Main “pack” module of the library. *)

open Efmp_internal_pervasives

module Unique_id = Unique_id

module Path = Path

module Configuration = Efmp_configuration

module Process = Efmp_unix_process

include Efmp_job_script

module Host = Efmp_host

include Efmp_meta_job

module Execution_engine = Efmp_execution_engine

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

  let display_info ~persist_with ~all ?item_format () =
    with_engine ~persist_with ~f:(fun engine ->
        display "%s" (Execution_engine.to_string ?item_format ~all engine)
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
        pure (fun persist_with all item_format ->
            display_info ~item_format ~all ~persist_with)
        $ persistence_file_arg
        $ Arg.(value & flag & info ["A"; "all"] 
                 ~doc:"Display all processes even the completed ones.")
        $ Arg.(value & 
               opt string "- $key:\n  $name\n  $status_with_reason\n$history_list"
               & info ["-F"; "item-format"] ~docv:"FORMAT-STRING"
                 ~doc:"Use $(docv) as format for displaying jobs")
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
                 while_sequential todos (fun todo ->
                     say "    %s" (Todo.to_string todo))
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



