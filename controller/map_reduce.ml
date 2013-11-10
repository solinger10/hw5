open Util
open Worker_manager


(* TODO implement these *)
let map kv_pairs map_filename : (string * string) list = 
  let wm = initialize_mappers map_filename in
  let input = Hashtbl.create (List.length kv_pairs) in
  let pool = Thread_pool.create 100 in
  let ans = ref [] in
  
  let lock = Mutex.create () in
  
  List.iter (fun (k,v) -> Hashtbl.add input k v) kv_pairs;


  let addwork (k,v) () = 
    let worker = pop_worker wm in 
	
	match (Worker_manager.map worker k v) with
    |None -> ()
    |Some l -> begin
      Mutex.lock lock;
    	  
      if not(Hashtbl.mem input k) then begin
        Mutex.unlock lock;
        Worker_manager.push_worker wm worker;
        end
      else begin
        Hashtbl.remove input k;
        ans:= List.fold_left (fun acc x -> x::acc) (!ans) l;
        Mutex.unlock lock;
    	Worker_manager.push_worker wm worker;
      end
	end
  in
  
  while Hashtbl.length input > 0
  do
    Hashtbl.iter (fun k v -> Thread_pool.add_work (addwork (k,v)) pool) input;
    Thread.delay 0.1
  done;
  Thread_pool.destroy pool;
  clean_up_workers wm;
  !ans
  

let combine kv_pairs : (string * string list) list = 
  failwith "You have been doomed ever since you lost the ability to love."

let reduce kvs_pairs reduce_filename : (string * string list) list =
  failwith "testing"
  


let map_reduce app_name mapper_name reducer_name kv_pairs =
  let map_filename    = Printf.sprintf "apps/%s/%s.ml" app_name mapper_name  in
  let reduce_filename = Printf.sprintf "apps/%s/%s.ml" app_name reducer_name in
  let mapped   = map kv_pairs map_filename in
  let combined = combine mapped in
  let reduced  = reduce combined reduce_filename in
  reduced

