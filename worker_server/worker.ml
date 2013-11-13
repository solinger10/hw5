open Protocol

let send_response client response =
  let success = Connection.output client response in
    (if not success then
      (Connection.close client;
       print_endline "Connection lost before response could be sent.")
    else ());
    success


let mappers = Hashtbl.create 100
let reducers = Hashtbl.create 100

let lock = Mutex.create()  

let rec handle_request client =
  match Connection.input client with
    | Some v -> begin
      match v with
      | InitMapper source -> begin
        match Program.build source with
        | (Some id, s) ->
          Mutex.lock lock;
          Hashtbl.add mappers id s;
          Mutex.unlock lock;
          if send_response client (Mapper (Some id, s))
          then handle_request client else ()
        | (None, s) -> 
        if send_response client (Mapper (None, s))
        then handle_request client else ()
        end
      | InitReducer source -> begin
        match Program.build source with
        | (Some id, s) ->
          Mutex.lock lock;
          Hashtbl.add reducers id s;
          Mutex.unlock lock;
          if send_response client (Reducer (Some id, s))
          then handle_request client else ()
        | (None, s) -> 
        if send_response client (Reducer (None, s))
        then handle_request client else ()
        end
      | MapRequest (id, k, v) -> 
        if Hashtbl.mem mappers id then begin 
          match Program.run id (k,v) with
          | None ->
            if send_response client (RuntimeError (id,"MapRequest error")) then
            handle_request client else ()
          | Some l -> if send_response client (MapResults (id,l)) then
            handle_request client else ()
        end
        else if send_response client (InvalidWorker id) then handle_request client
      | ReduceRequest (id, k, v) -> begin
        if Hashtbl.mem reducers id then begin 
          match Program.run id (k,v) with
          | None ->
            if send_response client (RuntimeError (id,"ReduceRequest error")) then
            handle_request client else ()
          | Some l -> if send_response client (ReduceResults (id,l)) then
            handle_request client else () end
        else if send_response client (InvalidWorker id) then
        handle_request client else () 
        end
      end
  | None ->
    Connection.close client;
    print_endline "Connection lost while waiting for request."

