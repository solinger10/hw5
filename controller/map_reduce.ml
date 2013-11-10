open Util
open Worker_manager
open Hashtbl 

let map kv_pairs map_filename : (string * string) list =
  failwith "Testing"
(*
(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  let wm = initialize_mappers map_filename in
  let input = Hashtbl.create (List.length kv_pairs) in
  let pool = Thread_pool.create 100 in
  
  let lock = mutex.create () in
  
  let addwork (k,v) = 
    let worker = pop_worker wm in 
	
	match (Worker_manager.map worker k v) with
	|None -> ()
	|Some l ->
      Mutex.lock lock;
	  if Hashtbl.mem input k then (*do nothing*)
	  else failwith "No" in
	  
  List.iter (fun (k,v) -> Hashtbl.add input k v) kv_pairs;
*)

let combine kv_pairs : (string * string list) list = 
  failwith "You have been doomed ever since you lost the ability to love."

let reduce kvs_pairs reduce_filename : (string * string list) list =
  let reduce_manager = Worker_manager.initialize_reducers reduce_filename in
  let threads = Thread_pool.create (List.length kvs_pairs) in
  let h_table = ref (Hashtbl.create List.length kvs_pairs) in
  let h_table_Mutex = Mutex.create in

  let reduce_one key values cur_worker =
    let result = Worker_manager.reduce cur_worker key values in
    Mutex.lock h_table_Mutex;
    Hashtbl.add !h_table key result;
    Mutex.unlock h_table_Mutex in
  
  (*Loops through current tasks, if done, remove from list, else add to waiting_list until done*)
  let rec check_Done (workers : worker list) (unfinished : worker list) : worker list = 
  match (workers, unfinished) with
  |(x::xs, _) -> Worker.send_request x (*Check if worker is done, if so remove from list, if not add to unfinished*)
  |([], _) -> check_Done unfinished [];
  |([], []) -> []

  (*Assigns a reducer worker a task based off a given kv_pair*)
  let assign workers kvs_pair : worker list =
  	match kvs_pair with
  	|(key, values) -> 
      begin
  		let cur_worker = Worker_manager.pop_worker reduce_manager in
      Thread_pool.add_work(reduce_one cur_worker key values) threads;
      cur_worker::workers
      end in
  let workers = List.fold_left assign [] kvs_pairs;


  let check_
  failwith "yeah"
  


let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

