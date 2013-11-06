open Util
open Worker_manager
open Thread_pool
open Hashtbl 

let h_table : Hashtbl ref = ref Hashtbl.create 97
let h_table_Mutex = Mutex.create ()

(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  let wm = initialize_mappers map_filename in
  let input = Hashtbl.create (List.length kv_pairs) in
  let pool = Thread_pool.create 100 in
  
  let lock = mutex.create () in
  
  List.iter (fun (k,v) -> Hashtbl.add input k v) kv_pairs;

  
  let addwork (k,v) = 
    let worker = pop_worker wm in 
	
	match (Worker_manager.map worker k v) with
	|None -> ()
	|Some l ->
      Mutex.lock lock;
	  if Hashtbl.mem input k then 
	  else 
	  

let combine kv_pairs : (string * string list) list = 
  failwith "You have been doomed ever since you lost the ability to love."

let reduce kvs_pairs reduce_filename : (string * string list) list =
  let reduce_manager = Worker_manager.initialize_reducers reduce_filename in
  let threads = Thread_pool.create (List.length kvs_pairs) in

  (*Appends the results of a worker to the hashtable, used for add_work*)
  let append_hashtable cur_worker key values cur_worker : unit = 
    Mutex.lock h_table_Mutex;
    let res_val = Worker_manager.reduce cur_worker key values 
    and new_table = !h_table in
      Hashtbl.replace new_table key res_val; 
      h_table := new_table; 
      Mutex.unlock h_table_Mutex in

  (*Iterates the kvs_pairs list, assigning a worker to each kvs pair*)
  let rec distribute kvs_pairs : unit =
  	match kvs_pairs with
  	|(key, values)::xs -> 
      begin
  		let cur_worker = Worker_manager.pop_worker reduce_manager in
      Thread_pool.add_work(append_hashtable cur_worker key values) threads;
      distribute xs
      end
  	|[] -> out_pairs in
  distribute kvs_pairs;
  


let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

