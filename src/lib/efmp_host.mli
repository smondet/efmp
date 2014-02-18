(** This module defines “hosts”: for now either the local machine, or a
      host accessible through SSH. *)

open Efmp_internal_pervasives

type connection = Efmp_data_t.connection
type t = host

val create :
  ?playground:Path.t ->
  ?qsub_is:string ->
  ?qstat_is:string ->
  ?qdel_is:string ->
  string ->
  connection ->
  t

val ssh :
  ?playground:Path.t ->
  ?qsub_is:string ->
  ?qstat_is:string ->
  ?qdel_is:string ->
  ?name:string ->
  ?port:int ->
  ?user:string ->
  string -> t


val name : t -> string
val to_string : t -> string

val playground_exn : t -> string

val tmp_on_localhost : t

val run_commands :
  configuration:configuration ->
  t ->
  string list ->
  ([ `Failure of string * string * string
   | `Success of string * string ],
   [> `process of
        [> `exec of string * string list ] * [> `exn of exn ] ])
    Deferred_result.t



val qsub_pbs_script :
  configuration:Efmp_configuration.t ->
  t ->
  Efmp_job_script.PBS_script.t ->
  ([ `Failure of string * string * string
   | `Success of string * string ] * string,
   [> `IO of
        [> `Write_file_exn of Efmp_internal_pervasives.IO.path * exn ]
   | `host of
        [> `missing_playground of Efmp_internal_pervasives.host ]
   | `process of
        [> `exec of string * string list ] *
        [> `exn of exn | `non_zero of string ] ])
    Deferred_result.t



val nohup_script :
  configuration:Efmp_configuration.t ->
  t ->
  Efmp_job_script.Nohup_script.t ->
  ([ `Failure of string * string * string
   | `Success of string * string ] * string,
   [> `host of [> `missing_playground of t ]
   | `process of
        [> `exec of string * string list ] *
          [> `exn of exn | `non_zero of string ]
   | `IO of
        [> `Write_file_exn of string * exn ]
   | `write_file_error of string * exn ])
    Deferred_result.t


val read_pbs_script_status :
  configuration:Efmp_configuration.t ->
  t ->
  playground:Path.t ->
  ([ `Log of
       [ `After of string * string * string
       | `Before of string * string * string
       | `Error of string list
       | `Failure of string * string * string
       | `Start of string
       | `Success of string ] list
   | `No_log ],
   'a)
    Deferred_result.t

val read_nohup_script_status :
  configuration:Efmp_configuration.t ->
  t ->
  playground:Path.t ->
  ([ `Log of
       [ `After of string * string * string
       | `Before of string * string * string
       | `Error of string list
       | `Failure of string * string * string
       | `Start of string
       | `Success of string ] list
   | `No_log ],
   'a)
    Deferred_result.t

val run_qdel:
  configuration:configuration ->
  t ->
  string ->
  ([ `Failure of string * string * string
   | `Success of string * string ],
   [> `process of
        [> `exec of string * string list ] *
          [> `exn of exn  ] ])
    Deferred_result.t


val kill_nohup_script :
  configuration:Efmp_configuration.t ->
  t ->
  playground:Path.t ->
  ([ `Failure of string * string * string
   | `Success of string * string ],
   [> `process of
        [> `exec of string * string list ] *
          [> `exn of exn  ] ])
    Deferred_result.t


(** Run `qstat` and interpret the result for any pbs id *)
val qstat :
  configuration:Efmp_configuration.t ->
  t ->
  string ->
  ([ `Dont_know | `Job_completed | `Job_queued | `Job_running ],
   [> `process of
        [> `exec of string * string list ] * [> `exn of exn ]
   | `qstat_parsing of string ])
    Deferred_result.t

val file_exists :
  t ->
  configuration:configuration ->
  path:Path.t ->
  (bool,
   [> `process of
        [> `exec of string * string list ] * [> `exn of exn ] ])
    Deferred_result.t












