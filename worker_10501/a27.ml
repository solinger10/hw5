open Util;;
let (key, values) = Program.get_input() in
let sorted = 
	List.sort (fun (_,id1,tid1) (_,id2,tid2) -> if id1 - id2 = 0 then tid1 - tid2 else id1 - id2) values in
let res = List.fold_left (fun acc (x,_) -> if x = Int64.zero then Int64.zero else Int64.add x acc) Int64.zero sorted in
Program.set_output [Int64.to_string res]
