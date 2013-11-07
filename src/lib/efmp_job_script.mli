(** Generate and manage PBS/Nohup monitored scripts. *)

open Efmp_internal_pervasives

(** Generate and manage “clever” Shell scripts *)
module Monitored_script: sig

  (** The goal of this module is the create shell scripts from a high-level
      representation; the scripts are “monitored” in the sense that code is
      added to log every returned value or failure in a parsable [log] file.
  *)

  type t = monitored_script
  (** See {!Efmp_data_t.monitored_script} *)

  val create: string list -> t

  val log_file : playground:string -> Path.t
  val pid_file : playground:string -> Path.t

  val to_string : playground:string -> t -> string
  (** Render the [monitored_script] to a shell-script string. *)

  val parse_log : string ->
    [ `After of string * string * string
    | `Before of string * string * string
    | `Error of string list
    | `Failure of string * string * string
    | `Start of string
    | `Success of string ] list
    (** Parse the log file of a [monitored_script] (the return type should change
        soon …). *)
end

(** PBS Wrapping of Monitored Scripts *)
module PBS_script: sig

  (** PBS scripts are here an enhanced and lazy version of the ones found in
      lthe library [ocaml-pbs], the library is then use to render them into
      files.  *)

  type t = pbs_script
  (** See {!Efmp_data_t.pbs_script} *)

  val create :
    name:string ->
    ?shell:string ->
    ?walltime:Efmp_internal_pervasives.timespan ->
    ?emailing:Efmp_internal_pervasives.emailing ->
    ?queue:string ->
    ?nodes:int ->
    ?ppn:int ->
    Monitored_script.t ->
    t
  (** Wrap a [monitored_script] in a PBS script. *)

  val log_file : playground:string -> Path.t
  (** Get the path to the script's log file. *)

  val to_string : t -> playground:string -> string
  (** Render the script to a string. *)

end

(** Monitored scripts intended to be called with [nohup setsid]. *)
module Nohup_script: sig

  (** This module is the parallel of [PBS_script] for monitored “nohup”-based
      scripts. *)
  type t = nohup_script

  val create : name:string -> Monitored_script.t -> t
  val err_file : playground:string -> string
  val out_file : playground:string -> string
  val to_string: t -> playground:string -> string

end
