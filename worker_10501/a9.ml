open Util;;
let (key, value) = Program.get_input() in
let block = split_spaces value in
let num_trans = int_of_string (List.hd block) and trans = List.tl block in
let rec format_block (t : string list) trans incnt outcnt acc 
	: (string * (int64 * int * int)) list = 
	match t with
	| a::b::xs -> 
		begin 
		let key = int_of_string key in
		if incnt > 0 then (*Formatting incoins*)
			begin
			format_block (b::xs) trans (incnt-1) outcnt 
				((a, (Int64.zero, key, num_trans-trans))::acc)
			end
		else if outcnt > 0 then (*Formatting outcoins*)
			begin
			format_block xs trans 0 (outcnt-1) 
				((a,(Int64.of_string b, key, num_trans - trans))::acc)
			end 
		else if trans > 0 then (*End of one transaction, start of another*)
			match t with
			| incnt::outcnt::xs -> 
				format_block xs (trans-1) (int_of_string incnt) (int_of_string outcnt) acc
			| [_] -> failwith "Malformed transaction"
			| [] -> acc
		else failwith "More transactions than given number"
		end
	| [_] -> failwith "Parsing error"
	| [] -> if trans > 0 then failwith "Fewer transactions than given number"
			else acc (*Completely done with block*)
in
Program.set_output (format_block trans num_trans 0 0 [])
