open Util
open Worker_manager
open Hashtbl 
open Thread_pool
open Hashtbl 

let h_table : Hashtbl ref = ref Hashtbl.create 97
let h_table_Mutex = Mutex.create ()


let map kv_pairs map_filename : (string * string) list =
  failwith "Testing"
(*
(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  let wm = initialize_mappers map_filename in
  let input = Hashtbl.create (List.length kv_pairs) in
  let pool = Thread_pool.create 100 in
  let ans = ref [] in
  
  let lock = mutex.create () in
  
  List.iter (fun (k,v) -> Hashtbl.add input k v) kv_pairs;


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
	  
	  if not(Hashtbl.mem input k) then Mutex.unlock lock
	  else 
	    Hashtbl.remove work k;
	    ans := l@!ans
	  


let combine kv_pairs : (string * string list) list = 
  failwith "You have been doomed ever since you lost the ability to love."

let reduce kvs_pairs reduce_filename : (string * string list) list =
  let reduce_manager = Worker_manager.initialize_reducers reduce_filename in
  let threads = Thread_pool.create 50) in
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

  let get_results : string * string list = 
    let append (res : string * string list) (kvspair : string * string list) 
    : (string * string list) = 
    match kvs_pair with
    |(key, _) -> (key, (Hashtbl.find (!h_table) key))::res in
    List.rev (List.fold_left append [] kvs_pairs)

  let workers = List.fold_left assign [] kvs_pairs;
  check_Done workers [];
  get_results

(*
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
*)

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

