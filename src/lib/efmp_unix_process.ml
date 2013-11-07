(** Manage external processes. *)

open Efmp_internal_pervasives

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

