(*
 * THREAD POOL - on creation, a set of worker threads are started.
 * Work to be done is added to the thread pool, and an available worker
 * thread gets to that work when it can. There are no guarantees about
 * when the work will be done or in what order. 
 *)

type pool

exception Destroying_pool
(**
 * [No_workers] exception is thrown if [add_work] is called when the
 * threadpool is being shut down. The work is not added. 
 *)

val create : int -> pool
(**
 * [create n] initializes a thread pool with [n] worker threads 
 *)

val add_work : (unit -> unit) -> pool -> unit
(**
 * [add_work job pool] puts [job] on the work queue for [pool].
 *)

val destroy : pool -> unit
(**
 * [destroy pool] stops all threads in [pool]. 
 * Waits for working threads to finish.
 *)
