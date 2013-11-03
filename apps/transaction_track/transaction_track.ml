open Util;;

let main (args : string array) : unit =
  if Array.length args < 3 then
    print_endline "Usage: transaction_track <filename>"
  else
    let filename = args.(2) in
    let docs = load_documents filename in
    let kv_pairs = List.rev_map (fun d -> (string_of_int d.id, d.body)) docs in
    let reduced = 
      Map_reduce.map_reduce "transaction_track" "mapper" "reducer" kv_pairs in
    let filtered = List.filter (fun (_,l) -> match l with | [] -> false | x::[] -> (int_of_string x) <> 0 |  _ -> true) reduced in
    print_reduced_documents filtered
in

main Sys.argv
