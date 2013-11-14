open Util;;
let (key, value) = Program.get_input() in
let to_kv_pair s = match split_spaces s with [k;v] -> (k,v) in
Program.set_output (List.fold_left (fun acc s -> (to_kv_pair s)::acc) []
					(split_to_class_lst value))
