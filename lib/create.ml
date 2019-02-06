(*-------------------------------- CREATE -------------------------------------*)

open Core_kernel

let buildTestPapers exam examProf =
  let seed' = String.to_array (examProf.Exam_j.prof_e_seed) in
  let seed = Array.map seed' ~f:(fun c -> Char.to_int c) in
  let rand = Core_kernel.Random.State.make seed in

  let groupList1 = List.map examProf.Exam_j.prof_e_profile
			    ~f:(fun grp ->
			        (grp.Exam_j.prof_g_name,grp.Exam_j.prof_g_num)
				) in
  let groupList2 = List.map exam.Exam_j.exam_e_groups
			    ~f:(fun grp ->
				 (grp.Exam_j.exam_g_name,grp.Exam_j.exam_g_questions)
				) in
  let groupList = List.map2_exn groupList1 groupList2
				~f:(fun pair1 pair2 ->
				    (fst pair1, snd pair1, snd pair2)
				   ) in
  let pickQuestions num question_list =
    let permuteAnswers question =
      {question with Exam_j.exam_q_answers=List.permute ~random_state:rand question.Exam_j.exam_q_answers} in

    let chosen_questions = List.take (List.permute ~random_state:rand question_list) num in
    List.map chosen_questions ~f:(fun question -> permuteAnswers question) in

  let buildPaper sn =
    let temp = List.map groupList ~f:(fun (_, num, qlist) ->
				      pickQuestions num qlist
				     ) in
    {Exam_j.paper_serial = sn; Exam_j.paper_questions = List.permute ~random_state:rand (List.concat temp)} in

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
			| -1 -> eprintf "Question %s has no correct answer...\n"
			               question.Exam_j.exam_q_name;
                               exit 1
		        | _ -> let parent = Option.value question.Exam_j.exam_q_parent ~default:("No parent") in
			       (Int.to_string index, parent) :: acc
		       ) in
  let temp = List.map papers
		      ~f:(fun paper ->
			  (Int.to_string paper.Exam_j.paper_serial, findCorrect paper)
			 ) in
  List.map temp
	   ~f:(fun (serial,alist) ->
	       let (a,b) = List.unzip alist in
	       (serial, a, b)
	      )
		      
	   
let buildAssocList correct_ans =
  let createAssoc serial a b =
    {Exam_j.assoc_serial = serial; Exam_j.assoc_groups = b; Exam_j.assoc_correct=a} in 
  List.map correct_ans
	   ~f:(fun (serial, a,b) ->
	       createAssoc serial a b
	      )
		 

let completeFields exam =
  let fixQ question grp_name =
    {question with Exam_j.exam_q_parent = Some grp_name} in

  let fixG group =
    let questions = List.map group.Exam_j.exam_g_questions
			     ~f:(fun question ->
				 fixQ question group.Exam_j.exam_g_name
				) in
    {group with Exam_j.exam_g_questions = questions} in
    
  let groups = List.map exam.Exam_j.exam_e_groups ~f:fixG in

  {exam with Exam_j.exam_e_groups=groups}


let create file =
  let info = Common.readFile file in
  let info'= Common.readFile Common.examProfileF in
  let exam' = try Exam_j.exam_of_string info with
	      | Yojson.Json_error _ -> eprintf "File %s is not a proper .json file.\n" file;
					       exit 1 in
  let exam = completeFields exam' in
  let data_exam = Exam_j.string_of_exam exam in 
  let examProf = Exam_j.examProfile_of_string info' in
  let testPapers = buildTestPapers exam examProf |> List.rev in
  let data_papers = Exam_j.string_of_testPapers testPapers in
  let correct_ans = buildCorrectAnswers testPapers in
  let data_correct_ans = List.map correct_ans
				  ~f:(fun (serial, a, _) ->
				      [serial; String.concat a]
				     ) in
  let correct_assoc = buildAssocList correct_ans in
  let data_correct_assoc = Exam_j.string_of_assocList correct_assoc in
				      
  Common.writeFile Common.testPapersF data_papers; 
  Common.writeFile Common.examF data_exam;
  Common.writeFile Common.assocF data_correct_assoc;
  Common.writeCSV Common.correctAnswersCSV data_correct_ans;
  Latex.writeTexFiles exam testPapers examProf;
  Latex.latexStuff Common.allQuestionsTex;
  Latex.latexStuff Common.testPapersTex;
  printf "\n Done!\n"
