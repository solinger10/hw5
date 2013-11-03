type pool = { mutable is_destroying : bool;
              mutable nworkers : int;
              queue : (unit -> unit) Queue.t;
              mutex : Mutex.t;
              work_c : Condition.t;
              destroying_c : Condition.t }

exception Destroying_pool

let do_work (tp : pool) : unit =
  let decrement_workers tp =
    Mutex.lock tp.mutex;
    tp.nworkers <- tp.nworkers - 1;
    (if tp.nworkers = 0 then Condition.signal tp.destroying_c);
    Mutex.unlock tp.mutex in
  let get_work tp =
    Mutex.lock tp.mutex;
    while (not tp.is_destroying) && (Queue.is_empty tp.queue) do
      Condition.wait tp.work_c tp.mutex
    done;
    let work =
      if tp.is_destroying then None
      else Some (Queue.take tp.queue) in
    Mutex.unlock tp.mutex;
    work in
  let rec perform_work tp =
    match get_work tp with
    | Some work_fun ->
      begin
        (try work_fun ()
         with ex ->
           print_endline
             ("Work in thread pool resulted in exception: " ^
             (Printexc.to_string ex)));
        perform_work tp
      end
    | None -> decrement_workers tp in
  perform_work tp

let create (size : int) : pool =
  if size < 1 then
    failwith "Thread_pool.create needs at least one thread"
  else
    let tp = { is_destroying = false;
               nworkers = 0;
               queue = Queue.create ();
               mutex = Mutex.create ();
               work_c = Condition.create ();
               destroying_c = Condition.create () } in
    Mutex.lock tp.mutex;
    while tp.nworkers < size do
      ignore (Thread.create do_work tp);
      tp.nworkers <- tp.nworkers + 1
    done;
    Mutex.unlock tp.mutex;
    tp

let add_work (work_fun : unit -> unit) (tp : pool) : unit =
  Mutex.lock tp.mutex;
  if tp.is_destroying then
    (Mutex.unlock tp.mutex;
     raise Destroying_pool)
  else
    (Queue.add work_fun tp.queue;
     Condition.signal tp.work_c;
     Mutex.unlock tp.mutex)

let destroy (tp : pool) : unit =
  Mutex.lock tp.mutex;
  Queue.clear tp.queue;
  tp.is_destroying <- true;
  Condition.broadcast tp.work_c;
  while tp.nworkers > 0 do
    Condition.wait tp.destroying_c tp.mutex
  done;
  Mutex.unlock tp.mutex
