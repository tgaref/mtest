(* model.ml - MultiTest Web Interface Model Module *)

open Core

(* Helper to copy file *)
let copy_file src dst =
  try
    let content = In_channel.read_all src in
    Out_channel.write_all dst ~data:content;
    true
  with _ -> false

(* Helper to recursively create directory *)
let rec mkdir_p path =
  try
    if not (Stdlib.Sys.file_exists path && Stdlib.Sys.is_directory path) then (
      let parent = Stdlib.Filename.dirname path in
      if not (String.equal parent path) then mkdir_p parent;
      Stdlib.Sys.mkdir path 0o755
    )
  with _ -> ()

(* Helper to copy directory contents *)
let copy_directory src dst =
  try
    if Stdlib.Sys.file_exists src && Stdlib.Sys.is_directory src then (
      if not (Stdlib.Sys.file_exists dst && Stdlib.Sys.is_directory dst) then
        mkdir_p dst;
      let files = Stdlib.Sys.readdir src in
      Array.iter files ~f:(fun f ->
        let s = Stdlib.Filename.concat src f in
        let d = Stdlib.Filename.concat dst f in
        if not (Stdlib.Sys.is_directory s) then (
          let _ = copy_file s d in ()
        )
      )
    )
  with _ -> ()

(* Subprocess runner to capture stdout/stderr *)
let run_command cmd =
  let log_file = "temp_output.log" in
  let full_cmd = Printf.sprintf "%s > %s 2>&1" cmd log_file in
  let exit_code = Stdlib.Sys.command full_cmd in
  let output =
    if Stdlib.Sys.file_exists log_file then (
      let content = In_channel.read_all log_file in
      (try Stdlib.Sys.remove log_file with _ -> ());
      content
    ) else ""
  in
  (exit_code, output)

(* Helper to extract only the last non-empty line of command output (to filter out GTK/Zenity warning logs) *)
let get_last_line s =
  let lines = String.split_lines s in
  let non_empty = List.filter lines ~f:(fun l -> String.length (String.strip l) > 0) in
  match List.last non_empty with
  | Some l -> String.strip l
  | None -> ""
