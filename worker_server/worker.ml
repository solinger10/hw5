open Protocol

let send_response client response =
  let success = Connection.output client response in
    (if not success then
      (Connection.close client;
       print_endline "Connection lost before response could be sent.")
    else ());
    success

type worker_type = Map | Reduce
let workers = ref (Hashtbl.create 50)
let workers_lock = Mutex.create ()

let rec handle_request client =
  match Connection.input client with
    Some v ->
      begin
        match v with
        | InitMapper source -> 
          begin
          match Program.build source with
            | (Some id, s) -> 
                Mutex.lock workers_lock;
                if Hashtbl.mem !workers id then 
                  begin
                  Mutex.unlock workers_lock;
                  send_response client (Mapper(None, s));
                  end
                else 
                  begin
                  Hashtbl.add !workers id Map;
                  Mutex.unlock workers_lock;
                  send_response client (Mapper(Some(id), s));
                  end
            | (None, s) -> send_response client (Mapper(None, s));
          end
        | InitReducer source -> 
          begin
          match Program.build source with
            | (Some id, s) -> 
              begin
              Mutex.lock workers_lock;
              if Hashtbl.mem !workers id then 
                begin
                Mutex.unlock workers_lock;
                send_response client (Reducer(None, s));
                end
              else 
                begin
                Hashtbl.add !workers id Reduce;
                Mutex.unlock workers_lock;
                send_response client (Reducer(Some(id), s));
                end
              end
            | (None, s) -> send_response client (Reducer(None, s));
          end
        | MapRequest (id, k, v) -> 
          if Hashtbl.mem !workers id && (Hashtbl.find !workers id) = Map then
            match Program.run id (k,v) with
            | Some result -> send_response client (MapResults(id, result));
            | None -> send_response client (RuntimeError(id, k));
          else 
            send_response client (InvalidWorker(id));
        | ReduceRequest (id, k, v) -> 
          if ((Hashtbl.mem !workers id) && ((Hashtbl.find !workers id) = Reduce)) then
            match Program.run id (k,v) with
            | Some result -> send_response client (ReduceResults(id, result));
            | None -> send_response client (RuntimeError(id, k));
          else 
            send_response client (InvalidWorker(id));
        handle_request client
      end
  | None ->
      Connection.close client;
      print_endline "Connection lost while waiting for request."

