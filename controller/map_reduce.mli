val map_reduce : string -> string -> string -> (string * string) list -> 
  (string * string list) list
(**
 * [map_reduce app_name mapper_name reducer_name kv_pairs] perform a
 * full map reduce job on [kv_pairs]. Dynamically loads the mapper
 * identified by the file path `[app_name]/[mapper_name].ml` and
 * the reducer identified by the file path `[app_name]/[reducer_name].ml`.
 *)
