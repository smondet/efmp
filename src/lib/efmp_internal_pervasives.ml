
(** The [doc] module is not expected to depend on any other library *)
module Doc = struct

  (* We need a GADT for the existential type of the `Apply_to_string` case *)
  type _ t =
    | Text:  string -> string t
    | Int:  int option * char * int -> string t
    | Float: int option * float -> string t
    | List: string * string * string * string t list -> string t
    | Concatenate: 'a t list -> 'a t
    | Apply_to_string: ('a -> string) * 'a -> string t
    | Be_lazy: ('a -> 'b t) * 'a -> 'b t
    | New_line: string t
    | Indent: int option * 'a t -> 'a t

  let (%) x y = Concatenate [x; y]
  let n = New_line
  let s x = Text x
  let d ?space ?(fill_with=' ') x = Int (space, fill_with, x)
  let f ?precision x = Float (precision, x)
  let l ?(sep="") ?(enclose="","") l =
    let left, right = enclose in
    List (sep, left, right, l)
  let a f x = Apply_to_string (f, x)
  let indent ?by x = Indent (by, x)

  let string_list x = 
    Be_lazy ((fun sl -> l ~sep:"; " ~enclose:("[", "]") (List.map s sl)), x)

  let string_list_map x ~f =
    Be_lazy (string_list, (List.map f x))

  let in_parens d = List ("", "(", ")", [d])
  let in_brackets d = List ("", "[", "]", [d])

  let rec write_aux ~indent ~f =
    let print_indented text =
      let current = ref 0 in
      let lgth = String.length text in
      try while true do
          let index = String.index_from text !current '\n' in
          f (String.sub text !current (!current - index - 1));
          f "\n";
          f (String.make indent ' ');
          current := index + 1;
        done;
      with _ -> 
        f (String.sub text !current (lgth - !current))
    in
    function
    | New_line -> f ("\n" ^ String.make indent ' ')
    | Apply_to_string (tos, x) -> print_indented (tos x)
    | Indent (by, x) ->
      let actual = (match by with None -> 4 | Some b -> b) in
      f (String.make (indent + actual) ' ');
      write_aux ~f ~indent:(indent + actual) x
    | Be_lazy (fo, o) -> write_aux ~f ~indent (fo o)
    | Concatenate l ->
      List.iter (write_aux ~f ~indent) l
    | Text t -> print_indented t
    | Int (space, fill_with, x) ->
      let s = string_of_int x in
      let l = String.length s in
      begin match space with
      | None -> f s
      | Some n when n = l  -> print_indented s
      | Some n when n < l  -> print_indented (String.sub s 0 (n - 1) ^ "#")
      | Some n (* n > l *) -> print_indented (String.make (n - l) fill_with ^ s)
      end
    | Float (precision, float) ->
      let s = string_of_float float in
      begin match precision with
      | None -> f s
      | Some i when i < 0 -> f s
      | Some 0 -> 
        begin try let idx = String.index s '.' in
          print_indented (String.sub s 0 (String.length s - idx))
        with _ -> print_indented s
        end
      | Some more -> 
        begin try let idx = String.index s '.' in
          f (String.sub s 0 (String.length s - idx + more))
        with _ -> print_indented s
        end
      end
    | List (sep, left, right, []) -> print_indented left; print_indented right
    | List (sep, left, right, h :: t) ->
      print_indented left;
      write_aux ~f ~indent h;
      List.iter (fun t -> print_indented sep; write_aux ~f ~indent t) t;
      print_indented right

  let to_string t =
    let b = Buffer.create 42 in
    write_aux t ~indent:0 ~f:(Buffer.add_string b);
    Buffer.contents b
  let (!=) t = to_string t


end

include Nonstd
module Result = Pvem.Result
include Pvem_lwt_unix
include Deferred_result
include Deferred_list
module String = struct
  include Sosa.Native_string
end

module Debug = struct
  include Doc
  let _global_debug_level = ref 0 (* 0 = print all *)
  let print level t = 
    if !_global_debug_level <= level
    then eprintf "## Debug {\n%s\n}\n%!" (to_string (indent t))
    else ()
  let (@) t level = print level t

  let verbose = 1
  let very_verbose = 0
end

module Time = struct
  let now () = Unix.gettimeofday ()

  let to_filename f =
    let open Unix in
    let tm = gmtime f in
    sprintf "%04d-%02d-%02d-%02dh%02dm%02ds%03dms-UTC"
      (tm.tm_year + 1900)
      (tm.tm_mon + 1)
      (tm.tm_mday)
      (tm.tm_hour + 1)
      (tm.tm_min + 1)
      (tm.tm_sec)
      ((f -. (floor f)) *. 1000. |> int_of_float)
end


include Efmp_data_t
module Data_j = Efmp_data_j
  

module Unique_id = struct
  (** Provide pseudo-unique identifiers. *)

  (** Create a fresh filename-compliant identifier. *)
  let create () =
    sprintf "efmp_%s_%09d"
      Time.(now () |> to_filename) (Random.int 1_000_000_000)
end

