open Util
open Worker_manager
open Hashtbl 
open Thread_pool
open Hashtbl 


let map kv_pairs map_filename : (string * string) list = 
  let wm = initialize_mappers map_filename in
  let input = Hashtbl.create (List.length kv_pairs) in
  let pool = Thread_pool.create 100 in
  let ans = ref [] in
  let lock = Mutex.create () in
  (*Puts every key value pair into the hashtable*)
  List.iter (fun (k,v) -> Hashtbl.add input k v) kv_pairs;

  let process (k,v) () = 
    let worker = pop_worker wm in 
    Printf.printf "Sending map request...";
    match (Worker_manager.map worker k v) with
    |None -> Printf.printf "Worker failure\n"; ()
    |Some l -> begin
      Mutex.lock lock;
      if not(Hashtbl.mem input k) then 
      (*Some other thread already got to it. Unlock mutex lock and readd worker*)
      begin
        Printf.printf "There is no %s key in the table\n" k;
        Mutex.unlock lock;
        Worker_manager.push_worker wm worker;
        end
      else
      (*Adds processed key value pairs to answer list*)
      begin
        Printf.printf "Removing key %s from table\n" k;
        Hashtbl.remove input k;
        ans:= List.fold_left (fun acc x -> x::acc) (!ans) l;
        Mutex.unlock lock;
      Worker_manager.push_worker wm worker;
      end
  end
  in
  
  (*Used to check if the hashtable has not been changed due to a bad key value pair*)
  let c = ref 0 in
  let old = ref 0 in
  
  (*List.iter (fun kv -> Thread_pool.add_work (process kv) pool) kv_pairs;*)
  while Hashtbl.length input > 0 do
    Printf.printf "Hashtable length: %n\n" (Hashtbl.length input);
    Printf.printf "Results list length: %n\n" (List.length !ans);

    if !old = Hashtbl.length input then c := !c+1 else c := 0;
    if !c > 20 then failwith "map bad key value pair" else
    old := Hashtbl.length input;
    Hashtbl.iter (fun k v -> 
          Printf.printf "Adding another thread to key %s\n" k;
          Thread_pool.add_work (process (k,v)) pool) input;
    Thread.delay 0.01
  done;
  Thread_pool.destroy pool;
  clean_up_workers wm;
  !ans
  
let combine kv_pairs : (string * string list) list = 
  let table = Hashtbl.create 100 in
  let process (k,v) = 
    if (Hashtbl.mem table k) then
      let l = (Hashtbl.find table k) in 
      Hashtbl.replace table k (v::l)
    else
      Hashtbl.add table k [v]
  in
  (*Adds each pair to the hashtable accounting for duplicate keys*)
  List.iter process kv_pairs;
  (*Creates list of tuples from the hashtable*)
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) table []

let reduce kvs_pairs reduce_filename : (string * string list) list =
  let wm = Worker_manager.initialize_reducers reduce_filename in
  let pool = Thread_pool.create 100 in
  let input = Hashtbl.create (List.length kvs_pairs) in
  let output = ref [] in
  let t_lock = Mutex.create () in
  let l_lock = Mutex.create () in
  List.iter (fun (k,v) -> Hashtbl.add input k v) kvs_pairs;
  
  let process (k,v) () =
    let worker = pop_worker wm in
    match Worker_manager.reduce worker k v with
    | Some res -> 
    begin
      Mutex.lock t_lock;
      if not(Hashtbl.mem input k) then
      begin
        (*kv_pair already evaluated, push worker back on queue*)
        Mutex.unlock t_lock;
        Worker_manager.push_worker wm worker;
      end
      else 
      begin
        (*kv_pair evaluated, store results, remove kv_pair from table*)
        Hashtbl.remove input k;
        Mutex.unlock t_lock;
        Mutex.lock l_lock;
        output := (k, res)::!output;
        Mutex.unlock l_lock;
        Worker_manager.push_worker wm worker;
      end
    end
    | None -> () in

  let count = ref 0 in
  let old = ref 0 in
  
  while Hashtbl.length input > 0 do
    if !old = Hashtbl.length input then count := !count+1 else count := 0;
    if !count > 15 then failwith "reduce bad key value pair" else
    old := Hashtbl.length input;
    Hashtbl.iter (fun k v -> Thread_pool.add_work (process (k,v)) pool) input;
    Thread.delay 0.1
  done;
  Thread_pool.destroy pool;
  clean_up_workers wm;
  !output

(*
  (*Assigns a reducer worker a task based off a given kv_pair*)
  let assign workers kvs_pair =
  	match kvs_pair with
  	|(key, values) -> 
      begin
  		let cur_worker = Worker_manager.pop_worker reduce_manager in
      Thread_pool.add_work (fun n -> reduce_one key values cur_worker) threads;
      (cur_worker, 0, key, values)::workers
      end in

  (*Loops through list of workers and their tasks, if the key of a task is undefined in the hashtable, the worker is still working, so wait. When the key is bound, remove the worker info from the list and push onto stack*)
  let rec check_Done processing unfinished = 
    match processing with
    |((worker_id, worker_conn), count, key, values)::xs -> 
      begin
      if Hashtbl.mem h_table key then 
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
*)

let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

