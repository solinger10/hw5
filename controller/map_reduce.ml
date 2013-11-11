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
    Mutex.lock h_table_Mutex;
    Hashtbl.add !h_table key result;
    Mutex.unlock h_table_Mutex in

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
        begin
        Worker_manager.push_worker reduce_manager (worker_id, worker_conn);
        check_Done xs unfinished
        end
      else if count > 25 then
        let new_worker = Worker_manager.pop_worker reduce_manager in
        Thread_pool.add_work(reduce_one new_worker key values);
        check_Done processing (new_worker, 0, key, values)::
        ((worker_id, worker_conn), 0, key, values)::unfinished
      else 
        let cur_worker = ((worker_id, worker_conn),count+1,key,values) in
        check_Done processing cur_worker::unfinished  
      end
    |[] -> if unfinished = [] then [] else check_Done unfinished [] in

  let get_results : string * string list = 
    let append (res : string * string list) (kvspair : string * string list) 
    : (string * string list) = 
    match kvs_pair with
    |(key, _) -> (key, (Hashtbl.find (!h_table) key))::res in
    List.rev (List.fold_left append [] kvs_pairs) in

  let workers = List.fold_left assign [] kvs_pairs in
  check_Done workers [];
  get_results

    (*
  (*Loops through current tasks, if done, remove from list, else add to waiting_list until done*)
  let rec check_Done (workers : worker list) (unfinished : worker list) : worker list = 
  match (workers, unfinished) with
  |(((worker_id, worker_conn), count, key, values)::xs, _) -> begin
    if Connection.output worker_conn (ReduceRequest (worker_id, key, values)) then begin
    match Connection.input worker_conn with 
    |Reducer(Some(id), work) -> (*Worker still reducing*)
      begin
      if count > 25 then 
        (*Taking too long, give work to another worker*)
        let new_worker = Worker_manager.pop_worker reduce_manager in
        Thread_pool.add_work(reduce_one key values cur_worker);
        check_Done xs ((Some(new_worker), 0, key, values)::(Some(cur_worker), 0, key, values)::unfinished)
      else check_Done xs (Some(cur_worker), count + 1, key, values)::unfinished 
      end
    |ReduceResults(Some(id), _) -> (*Worker done, don't add back to unfinished list, push on worker queue*) 
      begin
      Worker_manager.push_worker reduce_manager (worker_id, worker_conn); 
      check_Done xs unfinished 
      end
    |_ -> (*Worker failure, get new worker, don't push back to worker queue*) 
      begin
      let new_worker = Worker_manager.pop_worker reduce_manager in
      Thread_pool.add_work(append_hashtable new_worker key values);
      check_Done workers (Some(new_worker), 0)::unfinished
      end
  |([], _) -> check_Done unfinished []; (*Run again with unfinished*)
  |([], []) -> [] (*All reducers finished*)
*)

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

