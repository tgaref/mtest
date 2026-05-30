(*------------------------------ PROFILE ---------------------------------------*)

open Core

let create_profiles exam =
  let group_counts =
    List.fold exam.Exam_j.exam_e_questions ~init:(Map.empty (module String)) ~f:(fun acc q ->
      Map.update acc q.Exam_j.exam_q_group ~f:(function
        | None -> 1
        | Some n -> n + 1
      )
    )
  in
  let list =
    Map.to_alist group_counts
    |> List.map ~f:(fun (name, count) ->
         let a = {Exam_j.prof_g_name = name; Exam_j.prof_g_num = count; Exam_j.prof_g_saveSpace = false} in
         let b = {Exam_j.prof_mark_g_name = name; Exam_j.prof_mark_g_negmark=(-0.25); Exam_j.prof_mark_g_posmark=1.0} in
         (a, b)
       )
  in
  let (p_list, m_list) = List.unzip list in
  let e_prof = {Exam_j.prof_e_totalNum = 10; Exam_j.prof_e_seed = "1a2b3c4d"; Exam_j.prof_e_profile = p_list} in
  (e_prof, m_list)

let profile file =
  let info = Common.readFile file in
  let exam = try Exam_j.exam_of_string info with
	     | Yojson.Json_error _ -> eprintf "File %s is not a proper .json file.\n" file;
				      exit 1 in
  let (examProf,markProf) = create_profiles exam in
  let data = Exam_j.string_of_examProfile examProf in
  let data' = Exam_j.string_of_markProfile markProf in
  Common.writeFile Common.examProfileF data;
  Common.writeFile Common.markProfileF data';
  printf "\n Done!\n"
