(** Main engine running, and monitoring the jobs. *)

(** This module implements the basic functions that drive the actions
      (“[Todo.t]”). *)

open Efmp_internal_pervasives

type t = execution_engine

val to_json : t -> string

val of_json :
  string ->
  (t,  [> `Json_parsing of exn ]) Result.t

val create :
  ?ssh_style:[ `Dropbear | `Openssh ] ->
  unit -> t

val add_todo :
  t ->
  todo:todo ->
  (string, 'a) Deferred_result.t

val get_log : t -> (string * string) list

type runtime 

val make_runtime : unit -> runtime

val runtimes_to_string : runtime list -> string

val wake_up :
  t ->
  (runtime,
   [> `host of [> `missing_playground of host ]
   | `process of
        [> `exec of string * string list ] *
          [> `exn of exn | `non_zero of string ]
   | `qstat_parsing of string
   | `write_file_error of string * exn ])
    Deferred_result.t

val kill :
  t ->
  key:string ->
  (runtime,
   [> `process of [> `exec of string * string list ] * [> `exn of exn ]
   | `qstat_parsing of string ])
    Deferred_result.t

val to_string :
  ?item_format:string ->
  ?all:bool -> t -> string

