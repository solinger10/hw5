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

let empty_spec table b =
  b = ((bindings table) = [])

(* Dependent type for empty: 
 *
 * val empty : (table : ('a,'b) t)
            -> (b : bool where empty_spec table b)
 *)

(** Map.find ******************************************************************)

let find_spec table k x =
  if not (mem table k) then true
  else x = snd (List.find (fun (k', x') -> k' = k) (bindings table))

(* Dependent type for find: 
 *
 * val find : (table : ('a, 'b) t)
            -> (k : 'a)
            -> (x : 'b where find_spec table k x)
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

let subset l1 l2 = 
  List.for_all (fun x -> List.mem x l2) l1

let eqset l1 l2 = 
  subset l1 l2 && subset l1 l2

let bindings_without k table = 
  List.filter (fun (k', _) -> k <> k') (bindings table)

let remove_spec table k result = 
  eqset (bindings_without k table) (bindings results)
  
(* Dependent type for remove: 
 *
 * val remove : (k : 'a)
              -> (table : ('a, 'b) t)
              -> (result : ('a, 'b) t where remove_spec table k result)
 *)

(** Map.equal *****************************************************************)

let subset l1 l2 cmp = 
  List.for_all (fun x -> List.exists (cmp x) l2) l1

let eqset l1 l2 cmp = 
  subset l1 l2 cmp = subset l2 l1 cmp

let equal_spec cmp table1 table2 b =
  b = eqset (bindings table1) (bindings table2) cmp


(* Dependent type for equal: 
 * val equal : (cmp : ('a, 'b) -> ('a, 'b) -> bool)
            -> (table1 : ('a, 'b) t)
            -> (table2 : ('a, 'b) t)
            -> (b : bool where equal_spec cmp table1 table2 b)
 *)

end

(*
** vim: ts=2 sw=2 ai et
*)
