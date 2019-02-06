(*-------------------------------- LATEX -------------------------------------*)

open Core

let writeTexFiles exam papers examProf =
  let buildAssoc examProf =
  List.map examProf.Exam_j.prof_e_profile
	   ~f:(fun grp_prof ->
	       (grp_prof.Exam_j.prof_g_name, grp_prof.Exam_j.prof_g_saveSpace)
	      ) in
  let assoc_list = buildAssoc examProf in
  let latex_pre_handle = In_channel.create Common.latexPreampleF in
  let outh = Out_channel.create Common.allQuestionsTex in
  List.iter (In_channel.input_lines latex_pre_handle)
	    ~f:(fun line -> fprintf outh "%s\n" line);
  In_channel.close latex_pre_handle;
  fprintf outh "\\title{%s} \n"  exam.Exam_j.exam_e_name;
  fprintf outh "\\pagestyle{empty} \n";
  fprintf outh "\\begin{document} \n";
  fprintf outh "\\maketitle \n";
  List.iter exam.Exam_j.exam_e_groups
	   ~f:(fun grp ->
	       fprintf outh "\\flushleft \\underline{\\bf %s} \n" grp.Exam_j.exam_g_name;
               fprintf outh "\\begin{enumerate} \n";
	       List.iter grp.Exam_j.exam_g_questions
			 ~f:(fun qst ->
			     fprintf outh "\\item %s \n" qst.Exam_j.exam_q_statement;
                             fprintf outh "\\begin{enumerate} \n";
			     List.iter qst.Exam_j.exam_q_answers
				       ~f:(fun a ->
					   fprintf outh "\\item (%s)    %s\n" (Bool.to_string a.Exam_j.correct) a.Exam_j.ans
					  );
			     fprintf outh "\\end{enumerate} \n"
			    );
	       fprintf outh "\\end{enumerate} \n"
	      );
  fprintf outh "\\end{document}";
  Out_channel.close outh;

  let latex_pre_handle = In_channel.create Common.latexPreampleF in
  let outh = Out_channel.create Common.testPapersTex in
  List.iter (In_channel.input_lines latex_pre_handle)
	    ~f:(fun line -> fprintf outh "%s\n" line);
  In_channel.close latex_pre_handle;

  fprintf outh "\\usepackage{enumerate} \n";
  fprintf outh "\\pagestyle{empty} \n";
  fprintf outh "\\begin{document} \n";
  
  List.iter papers
	    ~f:(fun paper ->
		let numQ = List.length paper.Exam_j.paper_questions in
		let ansPerLine = if numQ >10 then 8 else numQ in
                let numLines = if numQ % ansPerLine = 0
			       then numQ / ansPerLine
			       else (numQ / ansPerLine) + 1 in

		fprintf outh "{\\Large\\bf %d} \\hspace{1.5cm}\n" paper.Exam_j.paper_serial;
                fprintf outh "%s\n" ("\\begin{tabular}{|"^(List.init ansPerLine
								       ~f:(fun _ ->
									   "l|"
									  )|>String.concat
							  )^"}");
		fprintf outh "%s\n" "\\hline";
		for l = 0 to numLines-1 do
		  fprintf outh "{\\large %d}: \\hspace*{0.5cm} \n" (l*ansPerLine+1);
		  for i = 2 to ansPerLine do
		    if l*ansPerLine+i<=numQ
		    then fprintf outh "& {\\large %d}: \\hspace*{0.5cm}" (l*ansPerLine+i)
		    else fprintf outh "& "
		  done;
		  fprintf outh "\\\\ \n";
		  fprintf outh "\\hline \n"
		done;
		fprintf outh "\\end{tabular} \n";
                fprintf outh "\\vspace*{1cm} \n \n";
                fprintf outh "{\\flushleft Κατεύθυνση: } \n";
                fprintf outh "{\\flushleft Όνομα/Α.Μ.: } \n";
                fprintf outh "\\vspace*{0.5cm} \n";
                fprintf outh "\\begin{center} {\\Large %s} \\end{center} \n" exam.Exam_j.exam_e_name; 
                fprintf outh "\\begin{enumerate} \n";
		List.iter paper.Exam_j.paper_questions
			  ~f:(fun qst ->
			      fprintf outh "\\item %s \n" qst.Exam_j.exam_q_statement;
			      let parent_grp =
				match qst.Exam_j.exam_q_parent with
				| None -> eprintf "Internal problem: no group name associated with question...";
					  exit 1
				| Some parent -> parent in
			      let save = List.Assoc.find_exn assoc_list ~equal:(String.equal) parent_grp in
			      match save with
			      | false -> fprintf outh "\\begin{enumerate}[(1)] \n";
					 List.iter qst.Exam_j.exam_q_answers
						   ~f:(fun a ->
						       fprintf outh "%s\n" ("    \\item "^a.Exam_j.ans)
						      );
					 fprintf outh "\\end{enumerate} \n";
			      | true -> fprintf outh " \n";
					let l = List.init (List.length qst.Exam_j.exam_q_answers)
							  ~f:(fun i ->  i+1) in
					let zipl = List.zip_exn l qst.Exam_j.exam_q_answers in
					List.iter zipl
						  ~f:(fun (i,a)->
						      fprintf outh "(%d) %s \\hspace{0.8cm}" i a.Exam_j.ans;
						     );
					fprintf outh " \n"
			     );
		fprintf outh "\\end{enumerate}\n";
                fprintf outh "\\hrulefill \\\\ \n";
		fprintf outh "%s\n" exam.Exam_j.exam_e_footer;
                fprintf outh "\\newpage \n";
	       );
  fprintf outh "\\end{document}";
  
  Out_channel.close outh

let latexStuff tex_file =
  let e=Sys.command ("xelatex --halt-on-error "^tex_file) in
  match e with
  |0 -> Sys.remove ((List.hd_exn (String.split ~on:'.' tex_file))^".aux");
	Sys.remove ((List.hd_exn (String.split ~on:'.' tex_file))^".log");
	()
  |_ -> Sys.remove ((List.hd_exn (String.split ~on:'.' tex_file))^".aux");
	Sys.remove ((List.hd_exn (String.split ~on:'.' tex_file))^".log");
        eprintf "Your latex file, %s ,is not properly formated." tex_file;
	exit 1
