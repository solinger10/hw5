open Protocol
module Pf = Printf

exception ManagerClosed

type worker_type = Map | Reduce

type worker = worker_id * Connection.connection
type mapper = worker
type reducer = worker

type 'a worker_manager = { worker_queue : 'a Queue.t;
                           mutex : Mutex.t;
                           worker_exists_c : Condition.t;
                           mutable closed : bool;
                           mutable workers : worker list}

let cADDRESS_FILENAME = "addresses"
let cNUM_RETRIES = 10

let addresses : Unix.sockaddr list =
  (* Break the file into lines. Each line should be `host:portnum` *)
  let address_strings = 
    Str.split (Str.regexp "\r?\n") (Util.read_whole_file cADDRESS_FILENAME) in
  let process_address l =
    match Str.split (Str.regexp ":") l with
    | [hostname; port] ->
        let host = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
        Unix.ADDR_INET (host, (int_of_string port))
    | _ -> 
      failwith "Invalid address file" in
  List.rev_map process_address address_strings

(* Cache mapper / reducer files rather than calling `Util.read_whole_file`
 * every time. Keys are filenames *)
let source_tbl = Hashtbl.create 16

let push_worker (wm : 'a worker_manager) (worker : 'a) : unit =
  Mutex.lock wm.mutex;
  if not wm.closed then
    (Queue.push worker wm.worker_queue;
     Condition.signal wm.worker_exists_c);
  Mutex.unlock wm.mutex

let add_new_worker (wm : 'a worker_manager) (worker : worker) : unit =
  Mutex.lock wm.mutex;
  wm.workers <- worker :: wm.workers;
  Mutex.unlock wm.mutex;
  push_worker wm worker

(* initializes either a mapper or a reducer, depending on worker_type *)
let initialize (worker_type : worker_type) (source_filename : string) : 'a worker_manager =
  let manager = { worker_queue = Queue.create ();
                  mutex = Mutex.create ();
                  worker_exists_c = Condition.create ();
                  closed = false;
                  workers = [] } in
  let connections =
    List.rev_map (fun a -> 
      Connection.init a cNUM_RETRIES) addresses in
  let source = 
    if Hashtbl.mem source_tbl source_filename 
    then Hashtbl.find source_tbl source_filename
    else begin
      let read = 
        Str.split (Str.regexp "\r?\n") (Util.read_whole_file source_filename) in
      Hashtbl.add source_tbl source_filename read;
      read end
  in
  let worker_request_wrapper =
    if worker_type = Map then InitMapper source
    else InitReducer source in
  let worker_response_wrapper connection =
    match Connection.input connection, worker_type with
    | Some (Mapper (Some id, _)), Map
    | Some (Reducer (Some id, _)), Reduce ->
        add_new_worker manager (id, connection)
    | Some (Mapper (_, error)), _
    | Some (Reducer (_, error)), _ ->
        Pf.eprintf "Compilation error: %s\n%!" error
    | None, _ ->
        prerr_endline "Failed to connect to worker"
    | _ ->
        prerr_endline "Worker returned incorrect message type"
  in
  List.iter (fun c ->
    match c with
    | Some connection ->
        if Connection.output connection worker_request_wrapper then
          ignore (Thread.create worker_response_wrapper connection)
        else
          prerr_endline "Worker did not respond in connection"
    | None -> ()) connections;
  manager

let initialize_mappers = initialize Map 
let initialize_reducers = initialize Reduce 

let pop_worker (wm : 'a worker_manager) : 'a =
  Mutex.lock wm.mutex;
  while Queue.is_empty wm.worker_queue && not wm.closed do
    Condition.wait wm.worker_exists_c wm.mutex
  done;
  if wm.closed then
    (Mutex.unlock wm.mutex;
     raise ManagerClosed)
  else
    (let element = Queue.pop wm.worker_queue in
     Mutex.unlock wm.mutex;
     element)

let send_request (id, connection) worker_type request =
  if Connection.output connection request then begin
    match Connection.input connection, worker_type with
    | Some (InvalidWorker id), _ ->
        Pf.eprintf "Invalid worker: %d\n%!" id;
        None
    | Some (RuntimeError (id, error)), _ ->
        Pf.eprintf "Runtime error for worker %d: %s\n%!" id error;
        None
    | Some (MapResults (id, l)), Map -> Some (MapResults (id, l))
    | Some (ReduceResults (id, l)), Reduce -> Some (ReduceResults (id, l))
    | None, _ -> begin
        Connection.close connection;
        Pf.eprintf "Connection lost to worker: %d\n%!" id;
        None end
    | Some (Reducer _), _
    | Some (Mapper _), _
    | Some (MapResults _), Reduce
    | Some (ReduceResults _), Map ->
        Pf.eprintf "Worker %d returned incorrect message type\n%!" id;
        None
    end
  else
    (Connection.close connection;
     Pf.eprintf "Connection lost to worker: %d\n%!" id;
     None)

let map (id, connection) key value =
  let request = MapRequest (id, key, value) in
  match send_request (id, connection) Map request with
  | None -> None
  | Some (MapResults (_, l)) -> Some l
  | _ -> failwith "Worker manager failed."

let reduce (id, connection) key values =
  let request = ReduceRequest (id, key, values) in
  match send_request (id, connection) Reduce request with
    None -> None
  | Some (ReduceResults (_, l)) -> Some l
  | _ -> failwith ("Worker manager failed.")

let clean_up_workers (wm : 'a worker_manager) : unit =
  Mutex.lock wm.mutex;
  wm.closed <- true;
  Queue.clear wm.worker_queue;
  List.iter (fun (_, connection) -> Connection.close connection) wm.workers;
  Condition.broadcast wm.worker_exists_c;
  Mutex.unlock wm.mutex
