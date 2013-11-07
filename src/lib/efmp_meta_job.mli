open Efmp_internal_pervasives

module Todo : sig
  type t = todo
  val to_string : t -> string
end

module Action : sig
  type t = action
  val create :
    ?host:Efmp_host.t ->
    action_edsl -> todo

  val get_output :
    ?host:Efmp_host.t ->
    name:string -> string list -> Todo.t

  val monitored_PBS :
    ?host:Efmp_host.t ->
    pbs_script -> Todo.t

  val monitored_nohup :
    ?host:Efmp_host.t ->
    nohup_script -> Todo.t
  val to_string : action -> string
  val to_json :
    ?len:int -> Data_j.action -> string
  val of_json :
    string ->
    [> `Error of [> `Json_parsing of exn ]
    | `Ok of Data_j.action ]
  val start :
    configuration:configuration ->
    action ->
    ([ `Fail_to_start_nohup_job of
         nohup_script * string * string * string * string
     | `Fail_to_start_pbs_job of
         pbs_script * string * string * string * string
     | `Failure_get_output of
         string * string list * string * string * string
     | `Start_nohup_job of nohup_script * string
     | `Start_pbs_job of pbs_script * string * string
     | `Success_empty
     | `Success_get_output of string * string list * string * string ],
     [> `host of [> `missing_playground of Efmp_host.t ]
     | `process of
          [> `exec of string * string list ] *
            [> `exn of exn | `non_zero of string ]
     | `write_file_error of string * exn ])
      Deferred_result.t
end

module Target : sig
  val file_exists :
    ?host:Efmp_host.t ->
    path -> target

  val check :
    configuration:configuration ->
    target ->
    ([> `Done | `To_build ],
     [> `process of [> `exec of string * string list ] * [> `exn of exn ] ])
      t
  val to_string : target -> string

end

module Make : sig

  type t = make
  val create :
    name:string ->
    target:target ->
    rule list -> Todo.t
  val to_string : make -> string
  val rule :
    ?name:string ->
    target:target ->
    todo:Todo.t ->
    target list -> rule
  val start_making_target :
    configuration:configuration ->
    make ->
    target:target ->
    ([> `Failed_to_make of (target * string) list
     | `Nothing_to_do of target
     | `Should_start of Todo.t list ],
     [> `process of [> `exec of string * string list ] * [> `exn of exn ] ])
      Deferred_result.t
  val start :
    configuration:configuration ->
    make ->
    ([> `Failed_to_make of (target * string) list
     | `Nothing_to_do of target
     | `Should_start of Todo.t list ],
     [> `process of [> `exec of string * string list ] * [> `exn of exn ] ])
      Deferred_result.t
end
