(* controller.ml - MultiTest Yume Web Interface Controller Module *)

open Core

(* Handle GET /style.css - serve the external stylesheet *)
let handle_style _env _req =
  (* Locate style.css relative to the working directory.
     When running from the project root the file lives at web-interface/style.css. *)
  let css_path = "web-interface/style.css" in
  (try
    let css = In_channel.read_all css_path in
    Yume.Server.respond
      ~headers:[(`Content_type, "text/css; charset=utf-8")]
      css
  with _ ->
    Yume.Server.respond
      ~status:`Not_found
      ~headers:[(`Content_type, "text/plain")]
      "/* style.css not found */")

(* Handle GET / request *)
let handle_index _env _req =
  let html = View.render_hub ~status_alert:"" ~console_log:"" ~generated_files:[] ~dest_dir:"" ~active_tab:"questions-editor-tab" in
  Yume.Server.respond_html html

(* Handle POST /run execution *)
let handle_run _env req =
  (* 1. Extract dest_dir *)
  let dest_dir =
    match Yume.Server.formdata "dest_dir" req with
    | Ok fd when String.length (String.strip fd.content) > 0 -> String.strip fd.content
    | _ -> "exam_backup"
  in

  (* 2. Extract active_tab *)
  let active_tab =
    match Yume.Server.formdata "active_tab" req with
    | Ok fd when String.length (String.strip fd.content) > 0 -> String.strip fd.content
    | _ -> "create-exam-tab"
  in

  (* 3. Extract action *)
  let action =
    match Yume.Server.formdata "action" req with
    | Ok fd -> fd.content
    | _ -> "create"
  in

  (* 4. Resolve questions file *)
  let questions_file =
    match Yume.Server.formdata "questions_path" req with
    | Ok fd when String.length (String.strip fd.content) > 0 -> String.strip fd.content
    | _ -> "example/questions.json"
  in

  (* 5. Prepare inputs in the Destination Directory *)
  let _ =
    Model.mkdir_p dest_dir;
    if String.equal action "create" then (
      (* Copy custom examProfile.json to dest_dir/examProfile.json if provided *)
      let target_profile = Stdlib.Filename.concat dest_dir "examProfile.json" in
      (match Yume.Server.formdata "exam_profile_path" req with
      | Ok fd when String.length (String.strip fd.content) > 0 ->
          let _ = Model.copy_file (String.strip fd.content) target_profile in ()
      | _ ->
          if not (Stdlib.Sys.file_exists target_profile) then
            let _ = Model.copy_file "example/examProfile.json" target_profile in ());
      
      (* Copy _typstPreamble_ to dest_dir if not exists *)
      let target_preample = Stdlib.Filename.concat dest_dir "_typstPreamble_" in
      if not (Stdlib.Sys.file_exists target_preample) then
        let _ = Model.copy_file "example/_typstPreamble_" target_preample in ()
        
    ) else if String.equal action "mark" then (
      (* Copy custom givenAnswers.csv to dest_dir/givenAnswers.csv if provided *)
      let target_answers = Stdlib.Filename.concat dest_dir "givenAnswers.csv" in
      let _ =
        match Yume.Server.formdata "given_answers_path" req with
        | Ok fd when String.length (String.strip fd.content) > 0 ->
            let _ = Model.copy_file (String.strip fd.content) target_answers in ()
        | _ ->
            if not (Stdlib.Sys.file_exists target_answers) then
              let _ = Model.copy_file "example/givenAnswers.csv" target_answers in ()
      in
      (* Copy custom markProfile.json to dest_dir/markProfile.json if provided *)
      let target_mark_profile = Stdlib.Filename.concat dest_dir "markProfile.json" in
      let _ =
        match Yume.Server.formdata "mark_profile_path" req with
        | Ok fd when String.length (String.strip fd.content) > 0 ->
            let _ = Model.copy_file (String.strip fd.content) target_mark_profile in ()
        | _ ->
            if not (Stdlib.Sys.file_exists target_mark_profile) then
              let _ = Model.copy_file "example/markProfile.json" target_mark_profile in ()
      in ()
    )
  in

  (* 6. Execute the binary as a sub-process *)
  let binary_path = "_build/default/bin/mtest.exe" in
  if not (Stdlib.Sys.file_exists binary_path) then
    let status_alert = {|
      <div class="alert alert-error">
        <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/></svg>
        Error: CLI binary is not compiled yet. Please build the project.
      </div>
    |} in
    let html = View.render_hub ~status_alert ~console_log:"Error: _build/default/bin/mtest.exe not found." ~generated_files:[] ~dest_dir ~active_tab in
    Yume.Server.respond_html html
  else
    let cmd =
      if String.equal action "create" then
        Printf.sprintf "%s create -dest %s %s" binary_path dest_dir questions_file
      else if String.equal action "mark" then
        Printf.sprintf "%s mark -dest %s %s" binary_path dest_dir (Stdlib.Filename.concat dest_dir "givenAnswers.csv")
      else
        Printf.sprintf "%s %s %s" binary_path action questions_file
    in
    let exit_code, console_log = Model.run_command cmd in

    (* If backup is successful, copy directory *)
    let _ =
      if exit_code = 0 && String.equal action "backup" then
        Model.copy_directory "exam_backup" dest_dir
    in

    let status_alert =
      if exit_code = 0 then
        Printf.sprintf {|
          <div class="alert alert-success">
            <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/></svg>
            Action '%s' completed successfully! Output copied to '%s'.
          </div>
        |} action dest_dir
      else
        Printf.sprintf {|
          <div class="alert alert-error">
            <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/></svg>
            Action '%s' failed with exit code %d. Review logs below.
          </div>
        |} action exit_code
    in

    (* Gather files in the target directory *)
    let generated_files =
      if Stdlib.Sys.file_exists dest_dir && Stdlib.Sys.is_directory dest_dir then
        Stdlib.Sys.readdir dest_dir |> Array.to_list |> List.sort ~compare:String.compare
      else []
    in

    let html = View.render_hub ~status_alert ~console_log ~generated_files ~dest_dir ~active_tab in
    Yume.Server.respond_html html

(* Handle GET /dirs query *)
let handle_dirs _env req =
  let current_path =
    match Yume.Server.query_opt "path" req with
    | Some p when String.length p > 0 -> p
    | _ -> "."
  in
  let absolute_path = current_path in
  let dirs =
    try
      let all = Stdlib.Sys.readdir absolute_path |> Array.to_list in
      let subdirs =
        all
        |> List.filter ~f:(fun f ->
             let full = Stdlib.Filename.concat absolute_path f in
             try Stdlib.Sys.is_directory full with _ -> false
           )
        |> List.sort ~compare:String.compare
      in
      if String.equal current_path "." || String.equal current_path "" then
        subdirs
      else
        ".." :: subdirs
    with _ -> []
  in
  let json = `List (List.map dirs ~f:(fun d -> `String d)) in
  Yume.Server.respond_yojson json

(* Handle GET /editor/load query *)
let handle_editor_load _env req =
  match Yume.Server.query_opt "file" req with
  | Some f when String.length f > 0 -> (
      try
        let content = In_channel.read_all f in
        let json = Yojson.Safe.from_string content in
        Yume.Server.respond_yojson json
      with ex ->
        let json = `Assoc [("status", `String "error"); ("message", `String (Stdlib.Printexc.to_string ex))] in
        Yume.Server.respond_yojson json
    )
  | _ ->
      let json = `Assoc [("status", `String "error"); ("message", `String "Missing file parameter")] in
      Yume.Server.respond_yojson json

(* Handle POST /editor/save execution *)
let handle_editor_save _env req =
  try
    let body_str = Yume.Server.body req in
    let json = Yojson.Safe.from_string body_str in
    let (filename, data) =
      match json with
      | `Assoc l ->
          let f =
            match List.Assoc.find l ~equal:String.equal "filename" with
            | Some (`String s) -> s
            | _ -> failwith "Missing or invalid filename"
          in
          let d =
            match List.Assoc.find l ~equal:String.equal "data" with
            | Some d -> d
            | _ -> failwith "Missing data"
          in
          (f, d)
      | _ -> failwith "Request body must be a JSON object"
    in
    
    let full_path =
      let with_ext =
        if String.is_suffix filename ~suffix:".json" then filename
        else filename ^ ".json"
      in
      if String.is_prefix with_ext ~prefix:"/" || Stdlib.String.contains with_ext '/' then
        with_ext
      else
        Stdlib.Filename.concat "example" with_ext
    in
    
    let _ =
      let dir = Stdlib.Filename.dirname full_path in
      if not (String.equal dir "." || String.equal dir "/") then (
        Model.mkdir_p dir
      )
    in
    
    let content = Yojson.Safe.pretty_to_string data in
    Out_channel.write_all full_path ~data:content;
    
    let resp = `Assoc [
      ("status", `String "ok");
      ("filename", `String full_path);
      ("message", `String ("Saved successfully to " ^ full_path))
    ] in
    Yume.Server.respond_yojson resp
  with ex ->
    let resp = `Assoc [
      ("status", `String "error");
      ("message", `String (Stdlib.Printexc.to_string ex))
    ] in
    Yume.Server.respond_yojson resp

(* Handle GET /pick-dir query *)
let handle_pick_dir _env _req =
  let cmd = "zenity --file-selection --directory --title=\"Select Destination Directory\"" in
  let exit_code, output = Model.run_command cmd in
  let path = Model.get_last_line output in
  let json =
    if exit_code = 0 && String.length path > 0 then
      `Assoc [("status", `String "ok"); ("path", `String path)]
    else
      `Assoc [("status", `String "error"); ("message", `String "Cancelled or failed")]
  in
  Yume.Server.respond_yojson json

(* Handle GET /pick-file query - supports filter and title params, and directory mode *)
let handle_pick_file _env req =
  let filter =
    match Yume.Server.query_opt "filter" req with
    | Some f when String.length f > 0 -> f
    | _ -> "*.json"
  in
  let title =
    match Yume.Server.query_opt "title" req with
    | Some t when String.length t > 0 -> t
    | _ -> "Select File"
  in
  let cmd =
    if String.equal filter "directory" then
      Printf.sprintf "zenity --file-selection --directory --title=\"%s\"" title
    else
      Printf.sprintf "zenity --file-selection --file-filter=\"%s\" --title=\"%s\"" filter title
  in
  let exit_code, output = Model.run_command cmd in
  let path = Model.get_last_line output in
  let json =
    if exit_code = 0 && String.length path > 0 then
      `Assoc [("status", `String "ok"); ("path", `String path)]
    else
      `Assoc [("status", `String "error"); ("message", `String "Cancelled or failed")]
  in
  Yume.Server.respond_yojson json

(* Handle GET /pick-save-file query *)
let handle_pick_save_file _env req =
  let default_filename =
    match Yume.Server.query_opt "default" req with
    | Some d when String.length d > 0 -> d
    | _ -> "my_questions.json"
  in
  (* Create parent directory recursively if it does not exist yet to prevent Zenity dialog failure *)
  let _ =
    let dir = Stdlib.Filename.dirname default_filename in
    if not (String.equal dir "." || String.equal dir "/") then (
      Model.mkdir_p dir
    )
  in
  let title =
    match Yume.Server.query_opt "title" req with
    | Some t when String.length t > 0 -> t
    | _ -> "Save JSON File"
  in
  let cmd =
    Printf.sprintf
      "zenity --file-selection --save --confirm-overwrite --file-filter=\"*.json\" --title=\"%s\" --filename=\"%s\""
      title default_filename
  in
  let exit_code, output = Model.run_command cmd in
  let path = Model.get_last_line output in
  let json =
    if exit_code = 0 && String.length path > 0 then
      `Assoc [("status", `String "ok"); ("path", `String path)]
    else
      `Assoc [("status", `String "error"); ("message", `String "Cancelled or failed")]
  in
  Yume.Server.respond_yojson json
