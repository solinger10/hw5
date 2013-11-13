open Util;;
let (key, values) = Program.get_input() in
let sorted = Array.sort (fun x y -> int_of_float (float_of_string x) -. (float_of_string y))
						(Array.of_list values) in
let length = Array.length sorted in
let median = 
	if length mod 2 = 1 then Array.get (length / 2)
	else ((float_of_string (Array.get (length / 2))) +. 
		(float_of_string (Array.get ((length / 2) + 1)))) /. 2. in
Program.set_output [string_of_float median]
