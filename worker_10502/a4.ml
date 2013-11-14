open Util;;
let (key, values) = Program.get_input() in
let sorted = 
	List.sort (fun (_,(_,id1)) (_,(_,id2)) -> id1 - id2) values in
let res = List.fold_left (fun acc x -> if x = Int64.zero then Int64.zero else Int64.add x acc) Int64.zero sorted in
Program.set_output [Int64.to_string res]
