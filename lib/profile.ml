(*------------------------------ PROFILE ---------------------------------------*)

open Core

let create_grp_profiles grp =
  let a =  {Exam_j.prof_g_name = grp.Exam_j.exam_g_name; Exam_j.prof_g_num = List.length grp.Exam_j.exam_g_questions; Exam_j.prof_g_saveSpace = false} in
  let b = {Exam_j.prof_mark_g_name = grp.Exam_j.exam_g_name; Exam_j.prof_mark_g_negmark=(-0.5); Exam_j.prof_mark_g_posmark=1.0} in
  (a,b)

let create_profiles exam =
  let list = List.map exam.Exam_j.exam_e_groups ~f:(create_grp_profiles) in
  let (p_list,m_list) = List.unzip list in
  let e_prof = {Exam_j.prof_e_totalNum = 10; Exam_j.prof_e_seed = "1a2b3c4d" ;Exam_j.prof_e_profile = p_list} in
  (e_prof,m_list) 

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

