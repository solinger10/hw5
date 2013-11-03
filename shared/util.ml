type document = {id : int; title : string; body : string}
type student = {id_num : int; course_grades : string}

type mass = Plane.scalar

type location  = Plane.point

type velocity  = Plane.vector

type body = mass * location * velocity

let cBIG_G : Plane.scalar = 6.67 *. (10.**(-11.))

(*
 * Computes word vectors for a set of documents. The given file should
 * contain a list of documents: one per line. Each document is of the
 * format: "id @ title @ body" such that '@' does not appear in the title
 * or body.
 *)
let load_documents (filename : string) : document list =
  let f = open_in filename in
  let rec next accum =
    match (try Some (input_line f) with End_of_file -> None) with
    | None -> accum
    | Some line ->
      (match Str.split (Str.regexp "@\\|$") line with
        | [id; title; body] ->
          next ({id = int_of_string id; title = title; body = body} :: accum)
        | _ -> failwith "malformed input") in
  let docs = next [] in
  close_in f;
  docs

let load_grades (filename : string) : student list = 
  let f = open_in filename in 
  let rec next acc = 
    match (try Some (input_line f) with End_of_file -> None) with
    | None -> acc
    | Some line ->
      (match Str.split (Str.regexp "@") line with
      | [id; grades] -> 
          next (({id_num = int_of_string id; course_grades = grades})::acc)
      | _ -> failwith "what kind of file are you giving me?!?")
  in
  let students = next [] in 
  close_in f;
  students 

let print_kvs (kvs_list : (string * string list) list) : unit =
  List.iter
    (fun (k, vs) ->
      let s = match vs with
        | [] -> ""
        | _ -> Printf.sprintf "'%s'" (String.concat "', '" vs) in
      Printf.printf "Key: {'%s'} Values: {%s}\n" k s)
    (List.sort (fun (k1, _) (k2, _) -> compare k1 k2) kvs_list)

let print_combine_results (kvs_list : (string * string list) list) : unit =
  print_endline "Combine Results";
  print_kvs kvs_list

let print_map_results (kv_list : (string * string) list) : unit =
  print_endline "Map Results";
  List.iter
    (fun (k, v) -> Printf.printf "Key: {'%s'} Value: {'%s'}\n" k v)
    (List.sort (fun (k1, _) (k2, _) -> compare k1 k2) kv_list)

let print_reduced_documents (kvs_list : (string * string list) list) : unit =
  print_endline "Reduce Results";
  print_kvs kvs_list

let print_reduced_courses (kvs_list : (string * string list) list) : unit =
  print_endline "Reduce Results";
  print_kvs kvs_list

(* Returns the entire contents of the provided filename *)
let read_whole_file filename =
  let file = open_in_bin filename in
  let size = in_channel_length file in
  let contents = String.create size in
  really_input file contents 0 size;
  close_in_noerr file;
  contents

(* Splits a string into words *)
let split_words = Str.split (Str.regexp "[^a-zA-Z0-9]+")
let split_spaces = Str.split (Str.regexp " ")
let split_to_class_lst = Str.split (Str.regexp "|") 

(* marshaling *)
let marshal x = Marshal.to_string x []
let unmarshal x = Marshal.from_string x 0
