(* Shared functionality between apps. 
 * The print functions might help in controller.exe *)
type document = {id : int; title : string; body : string}
type student = {id_num : int; course_grades : string}

val load_documents : string -> document list
(**
 * [load_documents filepath]
 * Computes word vectors for the set of documents located by [filepath]. 
 * The file at [filepath] must contain a list of documents, one per line. 
 * Each document is of the format: "id @ title @ body", 
 * such that '@' does not appear in the title
 * or body.
 *)

val load_grades : string -> student list

(* nbody types and values *)
type mass = Plane.scalar
type location  = Plane.point
type velocity  = Plane.vector
type body = Plane.scalar * Plane.point * Plane.vector
val cBIG_G : Plane.scalar

(** Debugging helpers for controller.exe *)
val print_kvs : (string * string list) list -> unit
val print_map_results : (string * string) list -> unit
val print_combine_results : (string * string list) list -> unit
val print_reduced_documents : (string * string list) list -> unit
val print_reduced_courses : (string * string list) list -> unit
 
(** File I/O helpers *)
val read_whole_file : string -> string
val split_words : string -> string list
val split_spaces : string -> string list
val split_to_class_lst : string -> string list

(** Wrapper for OCaml's [Marshal.to_string] *)
val marshal : 'a -> string

(** Wrapper for OCaml's Marshal.from_string *)
val unmarshal : string -> 'a

