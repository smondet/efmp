(** Manage the local configuration of an “EFMP {i instance}” *)

open Efmp_internal_pervasives

type t = configuration
(** See {!Efmp_data_t.configuration}. *)

val create : ?ssh_style:[ `Dropbear | `Openssh ] -> unit -> t
(** Create a new value of type [configuration]. *)

val ssh_batch_option : t -> string
(** Return a string which is the option to pass to the [ssh] command to
        avoid prompting for passwords. *)

val to_string : t -> string
(** Convert the configuration to a human-readable string. *)
