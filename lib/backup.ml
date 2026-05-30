(*-------------------------------- BACKUP --------------------------------------*)

open Core

let backup_dir = "exam_backup"

let copy_file src dst =
  let content = In_channel.read_all src in
  Out_channel.write_all dst ~data:content

let backup questions_file =
  if not (Stdlib.Sys.file_exists backup_dir && Stdlib.Sys.is_directory backup_dir) then
    Stdlib.Sys.mkdir backup_dir 0o755;
  
  let filenames = [
    questions_file;
    Common.examProfileF;
    Common.markProfileF;
    Common.testPapersF;
    Common.testPapersTex;
    Common.correctAnswersCSV;
    Common.assocF;
    Common.allQuestionsTex;
    Common.latexPreampleF;
  ] in

  List.iter filenames ~f:(fun f ->
    if Stdlib.Sys.file_exists f then (
      let dest = Stdlib.Filename.concat backup_dir (Stdlib.Filename.basename f) in
      copy_file f dest
    )
  );
  printf "\n Done!\n"
