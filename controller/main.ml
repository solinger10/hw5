let _ = 
  if Array.length Sys.argv <= 1 then
    (* List all non-hidden all directory names from apps directory *)
    let app_names = Array.fold_left (fun acc name -> 
      if name.[0] = '.' || not (Sys.is_directory (Printf.sprintf "apps/%s" name))
      then acc
      else Printf.sprintf "%s\n  - %s" acc name
    ) "" (Sys.readdir "apps/") in
    Printf.printf
      "Usage: `controller.exe <app_name> <args>`
  <args> are specific to the application <app_name>
  Options for <app_name> are:%s\n" app_names
  else 
    let app_name = Sys.argv.(1) in
    let app_path = Printf.sprintf "apps/%s/%s.cmo" app_name app_name in
    try
      Dynlink.loadfile_private app_path
    with
    | Dynlink.Error ex as e -> 
        print_endline (Dynlink.error_message ex);
        raise e
