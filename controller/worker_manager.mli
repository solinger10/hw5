(* Handles communication between workers and the master *)

type 'a worker_manager

type mapper

type reducer

val initialize_mappers : string -> mapper worker_manager
(** 
 * [initialize_mappers map_filename]: initializes mapper workers 
 *   at the addresses stored in the file named "addresses" that execute the code 
 *   stored in [map_filename].
 *)

val initialize_reducers : string  -> reducer worker_manager
(**
 * [initialize_reducers reduce_filename]: initializes reducer workers at the
 *   addresses stored in the file named "addresses" that execute the code
 *   stored in [reduce_filename].
 *)

val pop_worker : 'a worker_manager -> 'a
(**
 * [pop_worker wm]: removes a worker from the collection 
 *   [wm] of available workers.
 *   Blocks if there are no available workers.
 * Thread-safe
 * Returns: removed worker 
 *)

val push_worker : 'a worker_manager -> 'a -> unit
(**
 * [push_worker worker]: adds worker to the collection of available workers.
 * Thread-safe 
 *)

val map : mapper -> string -> string -> (string * string) list option
(**
 * [map worker key value]: sends a map request to the provided mapper with the
 *   (`key`, [value`) pair as input. 
 *   The function then blocks until the mapper responds.
 * Thread-safe
 * Returns: [Some l] where [l] is a list of the 
 *   (key, value) pairs computed by the mapper, 
 *   or [None] if the mapper experienced an error 
 *)

val reduce : reducer -> string -> string list -> string list option
(**
 * [reduce worker key values]: sends a reduce request to 
 *   the provided reducer with the (key, values) pair as input. 
 *   The function then blocks until the reducer responds.
 * Thread-safe
 * Returns: [Some l] where [l] is a list of the values 
 *   computed by the reducer,
 *   or [None] if the reducer experienced an error 
 *)

val clean_up_workers: 'a worker_manager -> unit
(**
 * [clean_up_workers manager]: closes all connections to workers.
 * Thread-safe
 *)
