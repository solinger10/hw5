open Util;;
let (key, values) = Program.get_input() in
let sorted = 
	List.sort (fun (_,(_,id1)) (_,(_,id2)) -> id1 - id2) values in
let res = List.fold_left (fun acc x -> if x = 0 then 0 else x + acc) sorted in
Program.set_output [string_of_int res]
