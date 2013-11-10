open Protocol

let send_response client response =
  let success = Connection.output client response in
    (if not success then
      (Connection.close client;
       print_endline "Connection lost before response could be sent.")
    else ());
    success

let mappers = ref []
let reducers = ref []

let rec handle_request client =
  match Connection.input client with
    Some v ->
      begin
        match v with
        | InitMapper source -> 
          match Program.build source with
            | (Some id, s) -> mappers := id::!mappers;
              send_response client (Mapper(Some(id), s));
            | (None, s) -> send_response client (Mapper(None, s))
        | InitReducer source -> 
          match Program.build source with
            | (Some id, s) -> reducers := id::!reducers;
              send_response client (Reducer(Some(id), s));
            | (None, s) -> send_response client (Reducer(None, s))
        | MapRequest (id, k, v) -> 
          if List.mem id !mappers then
            match Program.run id (k,v) with
            | Some result -> send_response client (MapResults(id, result);
            | None -> send_response client (RuntimeError(id, source));
          else 
            send_response client (InvalidWorker(id)); 
        | ReduceRequest (id, k, v) -> 
          if List.mem id !reducers then
            match Program.run id (k,v) with
            | Some result -> send_response client (ReduceResults(id, result);
            | None -> send_response client (RuntimeError(id, source));
          else 
            send_response client (InvalidWorker(id));
        handle_request client
      end
  | None ->
      Connection.close client;
      print_endline "Connection lost while waiting for request."

