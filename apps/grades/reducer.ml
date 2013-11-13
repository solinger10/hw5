open Util;;
let (key, values) = Program.get_input() in
let sorted = Array.of_list values in 
Array.sort (fun x y -> int_of_float ((float_of_string x)-.(float_of_string y))) 
	sorted;
let len = Array.length sorted in
let median = 
	if len mod 2 = 1 then float_of_string (Array.get sorted (len/2))
	else ((float_of_string (Array.get sorted ((len/2)-1))) +. 
		(float_of_string (Array.get sorted (len/2)))) /. 2. in
Program.set_output [string_of_float median]