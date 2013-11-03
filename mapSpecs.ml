module Make (Ord : Map.OrderedType) = struct

include Map.Make(Ord)

(* Complete the dependent type specifications in this file.  A dependent type
 * may make use of any functions that you have already given dependent types
 * for.  For example, the type for "mem" below depends on "bindings", which is
 * defined before it.
 *)

(** Map.bindings **************************************************************)

let rec is_sorted lst = match lst with
  | []       -> true
  | [x]      -> true
  | x::y::tl -> (Ord.compare x y < 0) && is_sorted (y::tl)

let bindings_spec lst = is_sorted (List.map fst lst)

(* Dependent type for bindings:
 *
 * val bindings : ('a, 'b) t -> (lst : ('a * 'b) list where bindings_spec lst)
 *)

(** Map.mem *******************************************************************)

let mem_spec table k b =
  b = List.exists (fun (k',_) -> k = k') (bindings table)

(* Dependent type for mem: 
 *
 * val mem : (table : ('a,'b) t)
 *        -> (k : 'a)
 *        -> (b : bool where mem_spec table k b)
 *)

(** Map.empty *****************************************************************)

let empty_spec =
  failwith "A true sign of intelligence is not knowledge but imagination."

(* Dependent type for empty: 
 *
 * val empty : TODO
 *)

(** Map.find ******************************************************************)

let find_spec =
  failwith "A person who never made a mistake never tried anything new."

(* Dependent type for find: 
 *
 * val find : TODO
 *)


(** Map.add *******************************************************************)

(* returns true if every member of l1 is a member of l2 *)
let subset l1 l2 =
  List.for_all (fun x -> List.mem x l2) l1

(* returns true if l1 and l2 contain the same elements *)
let eqset l1 l2 =
  subset l1 l2 && subset l2 l1

(* returns all bindings of table, except for the binding to k *)
let bindings_without k table =
  List.filter (fun (k',_) -> k <> k') (bindings table)

(* specification for add *)
let add_spec table k v result =
  eqset (bindings_without k table) (bindings_without k result)
  && mem  k result
  && find k result = v

(* Dependent type for add: 
 *
 * val add : (table : ('a,'b) t)
 *        -> (k:'a)
 *        -> (v:'b)
 *        -> (result : ('a,'b) t where add_spec table k v result)
 *)

(** Map.remove ****************************************************************)

let remove_spec = 
  failwith "I have no special talent, I am only passionately curious."

(* Dependent type for remove: 
 *
 * val remove : TODO
 *)

(** Map.equal *****************************************************************)


let equal_spec =
  failwith ("Insanity: doing the same thing over and over"^
            " again and expecting different results.")

(* Dependent type for equal: 
 * val equal : TODO
 *)

end

(*
** vim: ts=2 sw=2 ai et
*)
