open Protocol

let includes = "str.cma -I shared shared/util.cma"
let worker_dir = ref ""
let set_worker_dir dir = worker_dir := dir

let input_collection = Hashtbl.create 0
let output_collection = Hashtbl.create 0

let input_lock = Mutex.create()
let output_lock = Mutex.create()
let run_lock = Mutex.create()

let dynlink_init = ref false

let get_input () =
  Mutex.lock input_lock;
  let inp = Marshal.from_string (Hashtbl.find input_collection (Thread.self())) 0 in
    Mutex.unlock input_lock;
    inp

let set_input inp =
  Mutex.lock input_lock;
  Hashtbl.replace input_collection (Thread.self()) (Marshal.to_string inp []);
  Mutex.unlock input_lock

let get_output () =
  Mutex.lock output_lock;
  let out = Marshal.from_string (Hashtbl.find output_collection (Thread.self())) 0 in
  Mutex.unlock output_lock;
  out

let set_output out =
  Mutex.lock output_lock;
  Hashtbl.replace output_collection (Thread.self()) (Marshal.to_string out []);
  Mutex.unlock output_lock

(* Used to assign each compiled mapper/reducer a unique id *)
let counter = ref 0
let counter_lock = Mutex.create()
let next_id () =
  let _ = Mutex.lock counter_lock in
  let _ = counter := !counter + 1 in
  let id = !counter in
  let _ = Mutex.unlock counter_lock in id

let build source =
  if Sys.os_type <> "Win32" then Sys.set_signal Sys.sigchld Sys.Signal_default else ();
  let id = next_id () in
  let prefix = !worker_dir ^ "/a" ^ (string_of_int id) in
  let out = open_out (prefix ^ ".ml") in
  List.iter (fun x -> output_string out (x ^ "\n")) source;
  flush out;
  close_out_noerr out;
  let compile_cmd = Printf.sprintf
    "ocamlc -thread -c -I worker_server worker_server/program.cmo unix.cma threads.cma %s %s.ml 2> %s_build_output"
    includes prefix prefix in
  match Unix.system compile_cmd with
  | Unix.WEXITED code ->
      if code = 0 
      then (Some id, "")
      else let build_results = Util.read_whole_file (prefix ^ "_build_output") in
           (None, build_results)
  | _ -> (None, "Compilation failed due to system error")

let worker_files = ref [] 
let run id input_data = 
  set_input input_data;
  Mutex.lock run_lock;
  try
    if not !dynlink_init then
      begin
        let ocaml_lib = 
          match Sys.os_type with
          | "Win32" -> Sys.getenv "OCAMLLIB"
          | "Unix" ->
            let lib_dirs = ["/usr/lib/ocaml";
                            "/usr/local/lib/ocaml";
                            "/opt/local/lib/ocaml"] in
            (try List.find (fun f -> Sys.file_exists f && Sys.is_directory f) lib_dirs
             with Not_found ->
               let dirs = List.fold_left (fun f msg -> msg ^ "  " ^ f ^ "\n") "" lib_dirs in
               raise 
                 (Failure ("OCaml lib path not found.  Tried the following:\n" ^ dirs)))
          | "Cygwin" -> begin try
            Sys.getenv "OCAMLLIB" 
            with Not_found -> "/usr/lib/ocaml" end
          | _ -> raise (Failure "Error (program.ml/run): OS not supported. Contact the 3110 course staff.")
        in
        let extras = if Sys.os_type = "Win32" then Str.split (Str.regexp ";") (Sys.getenv "PATH") else [] in
        Dynlink.add_interfaces
          ["Pervasives"; "Util"; "Program"; "Thread"]
          ([ocaml_lib; "shared"; "worker_server"; ocaml_lib ^ "/threads"; Sys.getcwd()] @ extras);
        Dynlink.allow_unsafe_modules true;
        dynlink_init := true
      end
    else ();
    let worker_file = (!worker_dir ^ "/a" ^ (string_of_int id) ^ ".cmo") in
    Dynlink.loadfile worker_file;
    let result = get_output() in
    Mutex.unlock run_lock;
    Some result
  with
  | Not_found -> 
    Mutex.unlock run_lock; 
    None
  | Dynlink.Error e -> 
    Mutex.unlock run_lock;
    print_endline (Dynlink.error_message e);
    raise (Dynlink.Error e)
  | e -> 
    Mutex.unlock run_lock; 
    raise e
