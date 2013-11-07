(** Manage calls to Unix processes *)

open Efmp_internal_pervasives

(** High-level representation of Unix exit codes. *)
module Exit_code: sig
  type t = [
    | `Exited of int
    | `Signaled of int
    | `Stopped of int
  ]
  val to_string: t -> string
end

val exec :
  ?bin:string ->
  string list ->
  (string * string * Exit_code.t,
   [> `process of
        [> `exec of string * string list ] * [> `exn of exn ] ])
    Deferred_result.t
(** Execute a process with a given list of strings as “[argv]”, if you can
    provide the [~bin] argument to specify the actual file to be executed. The
    function returns the tuple [(stdout, stderr, exit_code)]. *)

val succeed :
  ?bin:string ->
  string list ->
  (string * string,
   [> `process of
        [> `exec of string * string list ] *
          [> `exn of exn | `non_zero of string ] ])
    Deferred_result.t
(** Do like {!exec} but fail if the process does not exit with [0] status. *)
