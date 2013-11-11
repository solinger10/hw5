open Util
open Worker_manager
open Hashtbl 
open Thread_pool
open Hashtbl 

let h_table = ref Hashtbl.create 97
let h_table_Mutex = Mutex.create ()

let map kv_pairs map_filename : (string * string) list = 
  failwith "Meow"
  
let combine kv_pairs : (string * string list) list = 
  failwith "You have been doomed ever since you lost the ability to love."

let reduce kvs_pairs reduce_filename : (string * string list) list =
  let reduce_manager = Worker_manager.initialize_reducers reduce_filename in
  let threads = Thread_pool.create 50 in
  let h_table = ref (Hashtbl.create List.length kvs_pairs) in
  let h_table_Mutex = Mutex.create in
  
  let reduce_one key values cur_worker =
    let result = Worker_manager.reduce cur_worker key values in
    match result with
    | Some(res) -> 
    Mutex.lock h_table_Mutex;
    Hashtbl.add !h_table key result;
    Mutex.unlock h_table_Mutex 
    | None -> ()
    in

  (*Assigns a reducer worker a task based off a given kv_pair*)
  let assign workers kvs_pair : worker list =
  	match kvs_pair with
  	|(key, values) -> 
      begin
  		let cur_worker = Worker_manager.pop_worker reduce_manager in
      Thread_pool.add_work(reduce_one cur_worker key values) threads;
      (cur_worker, 0, key, values)::workers
      end in

  (*Loops through list of workers and their tasks, if the key of a task is undefined in the hashtable, the worker is still working, so wait. When the key is bound, remove the worker info from the list and push onto stack*)
  let rec check_Done (processing : worker list) (unfinished : worker list) :
    worker list = 
    match processing with
    |((worker_id, worker_conn), count, key, values)::xs -> 
      begin
      if Hashtbl.mem key then 
        (*Worker done, push back on to queue*)
        begin
        Worker_manager.push_worker reduce_manager (worker_id, worker_conn);
        check_Done xs unfinished
        end
      else if count > 10 then
        (*Taking too long, assign new worker*)
        let new_worker = Worker_manager.pop_worker reduce_manager in
        Thread_pool.add_work(reduce_one new_worker key values);
        check_Done processing (new_worker, 0, key, values)::
        ((worker_id, worker_conn), 0, key, values)::unfinished
      else 
        (*Worker still working, put onto unfinished queue*)
        let cur_worker = ((worker_id, worker_conn),count+1,key,values) in
        check_Done processing cur_worker::unfinished  
      end
    |[] -> if unfinished = [] then [] else 
      Thread.delay 0.1; 
      check_Done unfinished [] in

  let get_results : string * string list = 
    let append (res : string * string list) (kvspair : string * string list) 
    : (string * string list) = 
    match kvs_pair with
    |(key, _) -> (key, (Hashtbl.find (!h_table) key))::res in
    List.rev (List.fold_left append [] kvs_pairs) in

  let workers = List.fold_left assign [] kvs_pairs in
  check_Done workers [];
  get_results

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

