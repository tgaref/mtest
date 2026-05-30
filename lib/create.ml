(*-------------------------------- CREATE -------------------------------------*)

open Core

let buildTestPapers exam examProf =
  let seed' = String.to_array (examProf.Exam_j.prof_e_seed) in
  let seed = Array.map seed' ~f:(fun c -> Char.to_int c) in
  let rand = Random.State.make seed in

  let questions_by_group =
    List.fold exam.Exam_j.exam_e_questions ~init:(Map.empty (module String)) ~f:(fun acc q ->
      Map.add_multi acc ~key:q.Exam_j.exam_q_group ~data:q
    )
  in

  let pickQuestions num question_list =
    let permuteAnswers question =
      {question with Exam_j.exam_q_answers=List.permute ~random_state:rand question.Exam_j.exam_q_answers} in
    let chosen_questions = List.take (List.permute ~random_state:rand question_list) num in
    List.map chosen_questions ~f:permuteAnswers
  in

  let buildPaper sn =
    let temp = List.map examProf.Exam_j.prof_e_profile ~f:(fun grp_prof ->
                 let qlist = Option.value (Map.find questions_by_group grp_prof.Exam_j.prof_g_name) ~default:[] in
                 pickQuestions grp_prof.Exam_j.prof_g_num qlist
               ) in
    {Exam_j.paper_serial = sn; Exam_j.paper_questions = List.permute ~random_state:rand (List.concat temp)}
  in

  let rec build n =
    match n with
    | 0 -> []
    | n -> buildPaper n :: (build (n-1)) in

  build (examProf.Exam_j.prof_e_totalNum)


let buildCorrectAnswers papers =
  let rec findIndex ~f list n =
    match list with
    | []      -> -1
    | (hd::tl) -> if (f hd) then n else findIndex ~f tl (n+1) in
		   
  let findCorrect paper =
    List.fold_right paper.Exam_j.paper_questions
		    ~init:([])
		    ~f:(fun question acc ->
			let index = findIndex question.Exam_j.exam_q_answers 1
				        ~f:(fun answer ->
					    answer.Exam_j.correct
					   ) in
			match index with
			| -1 -> eprintf "Question in paper %d has no correct answer...\n" paper.Exam_j.paper_serial;
                               exit 1
		        | _ -> (Int.to_string index, question.Exam_j.exam_q_group) :: acc
		       ) in
  List.map papers
	   ~f:(fun paper ->
	       { Exam_j.corr_serial = Int.to_string paper.Exam_j.paper_serial;
	         Exam_j.corr_answers = findCorrect paper;
	       }
	      )


let create file =
  let info = Common.readFile file in
  let info'= Common.readFile Common.examProfileF in
  let exam = try Exam_j.exam_of_string info with
	      | Yojson.Json_error _ -> eprintf "File %s is not a proper .json file.\n" file;
					       exit 1 in
  let examProf = Exam_j.examProfile_of_string info' in
  let testPapers = buildTestPapers exam examProf |> List.rev in
  let data_papers = Exam_j.string_of_testPapers testPapers in
  
  let correct_ans = buildCorrectAnswers testPapers in
  let data_correct_ans_json = Exam_j.string_of_correctAnsList correct_ans in
  
  let data_correct_ans_csv = List.map correct_ans ~f:(fun ca ->
    let serial = ca.Exam_j.corr_serial in
    let ans_strs = List.map ca.Exam_j.corr_answers ~f:(fun (index, _grp) -> index) in
    [serial; String.concat ans_strs]
  ) in
				      
  Common.writeFile Common.testPapersF data_papers; 
  Common.writeFile Common.assocF data_correct_ans_json;
  Common.writeCSV Common.correctAnswersCSV data_correct_ans_csv;
  Latex.writeTexFiles exam testPapers examProf;
  Latex.latexStuff Common.allQuestionsTex;
  Latex.latexStuff Common.testPapersTex;
  printf "\n Done!\n"
