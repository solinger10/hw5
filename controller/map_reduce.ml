open Util
open Worker_manager

(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  let manager = initialize_mappers map_filename in
  let input = Hashtbl.create (List.length kv_pairs) in
  let pool = Thread_pool.create 100 in
  
  List.iter (fun (k,v) -> Hashtbl.add input k v) kv_pairs;

let combine kv_pairs : (string * string list) list = 
  failwith "You have been doomed ever since you lost the ability to love."

let reduce kvs_pairs reduce_filename : (string * string list) list =
  failwith "The only thing necessary for evil to triumph is for good men to do nothing"

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

