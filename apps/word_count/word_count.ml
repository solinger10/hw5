open Util;;

let main (args : string array) : unit =
  if Array.length args < 3 then
    print_endline "Usage: word_count <filename>"
  else
    let filename = args.(2) in
    let docs = load_documents filename in
    let kv_pairs = List.rev_map (fun d -> (string_of_int d.id, d.body)) docs in
    let reduced = 
      Map_reduce.map_reduce "word_count" "mapper" "reducer" kv_pairs in
    print_reduced_documents reduced 
in

main Sys.argv
