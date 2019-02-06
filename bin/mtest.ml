(*-------------------------------- MAIN ----------------------------------------*)

open Core		    

let regular_file =
  Command.Spec.Arg_type.create
    (fun filename ->
     match Sys.is_file filename with
     | `Yes -> filename
     | `No | `Unknown ->
	      eprintf "File '%s' does not exist\n%!" filename;
              exit 1
  )

let profile =
  Command.basic
  ~summary: "Create a test profile using [File]"
  Command.Let_syntax.(
    let%map_open filename = anon ("questions file" %: regular_file) in
    fun () ->
    Lib.Profile.profile filename
  )

let create =
  Command.basic
  ~summary: "Create test papers using [File]"
  Command.Let_syntax.(
    let%map_open filename = anon ("questions file" %: regular_file) in
    fun () ->
    Lib.Create.create filename
  )

let mark =
  Command.basic
  ~summary: "Mark test papers using [File]"
  Command.Let_syntax.(
    let%map_open filename = anon ("questions file" %: regular_file) in
    fun () ->
    Lib.Mark.mark filename
  )

  
let command =
  Command.group
  ~summary:" A tool for creating and marking multiple choice tests"
  ~readme:(fun () -> "A tool for creating and marking multiple choice tests")
  [ "profile", profile
  ; "create", create
  ; "mark", mark]

let () =
  Command.run ~version:"0.1" ~build_info:"TG" command

