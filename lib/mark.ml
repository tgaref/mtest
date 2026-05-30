(*-------------------------------- MARK -----------------------------------------*)

open Core

let mark file =
  let given_ans' = Common.readCSV file in
  let given_ans = List.map given_ans'
			   ~f:(fun list ->
			       match list with
			       | []|[_]|[_;_] -> eprintf "File %s is missing information.\n" file;
						 exit 1;
			       | serial::am::ans::_ -> let ans_list = String.to_list ans in
							let ans_list'=List.map ans_list
									       ~f:(fun c ->
										   String.of_char c
										  ) in
							(serial,am,ans_list')
			      ) in
  let info = Common.readFile Common.assocF in
  let correct_assoc = Exam_j.correctAnsList_of_string info in
  let correct_alist = List.map correct_assoc
				   ~f:(fun r ->
				       let serial = r.Exam_j.corr_serial in
				       let answers = r.Exam_j.corr_answers in
				       (serial, answers)
				      ) in
  let correct_map = Map.of_alist_exn (module String) correct_alist in
  let info = Common.readFile Common.markProfileF in
  let markProf = Exam_j.markProfile_of_string info in
  let mark_alist = List.map markProf
				   ~f:(fun r ->
				       let name = r.Exam_j.prof_mark_g_name in
				       let neg = r.Exam_j.prof_mark_g_negmark in
				       let pos = r.Exam_j.prof_mark_g_posmark in
				       (name,(neg,pos))
				      ) in
  let mark_map = Map.of_alist_exn (module String) mark_alist in
  let results = List.map given_ans
			 ~f:(fun (serial,am,ans_list) ->
			     let correct_answers = Map.find_exn correct_map serial in
			     let zip_option = List.zip ans_list correct_answers in
			     match zip_option with
			     | Unequal_lengths -> eprintf "Incorrect number of answers for AM: %s\n" am;
				       exit 1
			     | Ok zip -> let m = List.fold zip
					   ~init:0.0
					   ~f:(fun acc (given,(cor,grp)) ->
					       let (neg,pos) = Map.find_exn mark_map grp in
					       if (String.equal given "x" || String.equal given "0")
					       then acc
					       else if String.equal given cor
					       then acc+.pos
					       else acc+.neg
					      ) in
					   let m'=
					     let open Float.O in
					     if m < 0.0 then 0.0 else m
					   in
					   [am;serial; String.concat ans_list; String.concat (fst (List.unzip correct_answers));Float.to_string m']
			    ) in
  printf "\nDone!\n";
  Common.writeCSV Common.resultsCSV results
