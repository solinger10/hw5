open Util;;
let (key, values) = Program.get_input() in
let sorted = List.sort 
	(fun x y -> int_of_float ((float_of_string x) -. (float_of_string y))) 
	values in
let len = List.length sorted in
let median = 
	if len mod 2 = 1 then float_of_string (List.nth sorted (len/2))
	else ((float_of_string (List.nth sorted ((len/2)-1))) +. 
		(float_of_string (List.nth sorted (len/2)))) /. 2. in
Program.set_output [string_of_float median]
