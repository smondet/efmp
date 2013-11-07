open Efmp_internal_pervasives

(** This module handles the “local” configuration, the configuration of the
      host from where the commands are run. *)

type t = configuration

let create ?(ssh_style=`Openssh) () = {ssh_style}

let to_json = Data_j.string_of_configuration
let of_json s =
  try `Ok (Data_j.configuration_of_string s)
  with e -> `Error (`Json_parsing e)

(** Return a string which is the option to pass to the [ssh] command to
        avoid prompting for passwords. *)
let ssh_batch_option t =
  match t.ssh_style with
  | `Openssh -> "-oBatchMode=yes"
  | `Dropbear -> "-s"

let to_string t =
  sprintf "ssh-style: %s"
    (match t.ssh_style with
     | `Openssh -> "OpenSSH"
     | `Dropbear -> "Dropbear")
