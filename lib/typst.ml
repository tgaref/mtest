(*-------------------------------- TYPST -------------------------------------*)

open Core

let translate_to_typst_string s =
  let s = String.substr_replace_all s ~pattern:"\\binom" ~with_:"binom" in
  let s = String.substr_replace_all s ~pattern:"\\frac" ~with_:"frac" in
  let s = String.substr_replace_all s ~pattern:"\\cdot" ~with_:" dot.c " in
  let s = String.substr_replace_all s ~pattern:"\\mathbb" ~with_:"bb" in
  let s = String.substr_replace_all s ~pattern:"\\mathbf" ~with_:"bf" in
  let s = String.substr_replace_all s ~pattern:"\\ff" ~with_:"ff" in
  let s = String.substr_replace_all s ~pattern:"\\fl" ~with_:"fl" in
  let s = String.substr_replace_all s ~pattern:"\\mat" ~with_:"mat" in
  let s = String.substr_replace_all s ~pattern:"\\Tr" ~with_:"Tr" in
  let s = String.substr_replace_all s ~pattern:"\\an" ~with_:"an" in
  let s = String.substr_replace_all s ~pattern:"\\Aut" ~with_:"Aut" in
  let s = String.substr_replace_all s ~pattern:"\\id" ~with_:"id" in
  let s = String.substr_replace_all s ~pattern:"\\im" ~with_:"im" in
  let s = String.substr_replace_all s ~pattern:"\\mkd" ~with_:"mkd" in
  let s = String.substr_replace_all s ~pattern:"\\ekp" ~with_:"ekp" in
  
  let parts = String.split s ~on:'$' in
  let translated_parts = List.mapi parts ~f:(fun idx part ->
    if idx % 2 = 1 then (
      let part = String.substr_replace_all part ~pattern:"}{" ~with_:", " in
      let part = String.substr_replace_all part ~pattern:"{" ~with_:"(" in
      let part = String.substr_replace_all part ~pattern:"}" ~with_:")" in
      let part = String.substr_replace_all part ~pattern:"\\" ~with_:"" in
      part
    ) else (
      part
    )
  ) in
  String.concat ~sep:"$" translated_parts

let writeTypstFiles ?dest_dir exam papers examProf =
  let resolve_path path =
    match dest_dir with
    | Some dir -> Stdlib.Filename.concat dir path
    | None -> path
  in
  let buildAssoc examProf =
    List.map examProf.Exam_j.prof_e_profile
	   ~f:(fun grp_prof ->
	       (grp_prof.Exam_j.prof_g_name, grp_prof.Exam_j.prof_g_saveSpace)
	      ) in
  let assoc_list = buildAssoc examProf in
  let typst_pre_path =
    match dest_dir with
    | Some dir when Stdlib.Sys.file_exists (Stdlib.Filename.concat dir "_typstPreamble_") ->
        Stdlib.Filename.concat dir "_typstPreamble_"
    | _ ->
        if Stdlib.Sys.file_exists "_typstPreamble_" then "_typstPreamble_"
        else "example/_typstPreamble_"
  in
  let typst_pre_content =
    if Stdlib.Sys.file_exists typst_pre_path then
      String.concat ~sep:"\n" (In_channel.read_lines typst_pre_path)
    else
      "#set page(paper: \"a4\", margin: (top: 2cm, bottom: 2cm, left: 2cm, right: 2cm))\n" ^
      "#set text(font: (\"Liberation Serif\", \"Linux Libertine\", \"Times New Roman\"), size: 11pt)\n" ^
      "#let frac(a, b) = $a/b$\n" ^
      "#let binom(a, b) = math.binom(a, b)\n" ^
      "#let fl(x) = math.floor(x)\n" ^
      "#let ff(q) = $bb(F)_#q$\n" ^
      "#let mat = $upright(\"Mat\")$\n" ^
      "#let Tr = $upright(\"Tr\")$\n" ^
      "#let F = $bb(F)$\n" ^
      "#let J = $bb(J)$\n" ^
      "#let D = $bb(D)$\n" ^
      "#let N = $bb(N)$\n" ^
      "#let Jac = $bb(J)$\n" ^
      "#let R = $bb(R)$\n" ^
      "#let Q = $bb(Q)$\n" ^
      "#let C = $bb(C)$\n" ^
      "#let Z = $bb(Z)$\n" ^
      "#let de = $upright(\"det\")$\n" ^
      "#let Gal = $upright(\"Gal\")$\n" ^
      "#let cB = $cal(B)$\n" ^
      "#let cN = $cal(N)$\n" ^
      "#let cR = $cal(R)$\n" ^
      "#let bb_b = $bold(b)$\n" ^
      "#let bx = $bold(x)$\n" ^
      "#let an(x) = $angle.l #x angle.r$\n" ^
      "#let Aut = $upright(\"Aut\")$\n" ^
      "#let id = $upright(\"id\")$\n" ^
      "#let im = $upright(\"im\")$\n" ^
      "#let mkd = $upright(mu kappa delta)$\n" ^
      "#let ekp = $upright(epsilon kappa pi)$\n"
  in

  (* Write ALL_QUESTIONS.typ *)
  let outh = Out_channel.create (resolve_path "ALL_QUESTIONS.typ") in
  fprintf outh "%s\n" typst_pre_content;
  fprintf outh "#align(center)[#text(size: 18pt, weight: \"bold\")[%s]]\n\n" (translate_to_typst_string exam.Exam_j.exam_e_name);

  let questions_by_group =
    List.fold exam.Exam_j.exam_e_questions ~init:(Map.empty (module String)) ~f:(fun acc q ->
      Map.add_multi acc ~key:q.Exam_j.exam_q_group ~data:q
    )
  in

  Map.iteri questions_by_group ~f:(fun ~key:group_name ~data:questions ->
    fprintf outh "== %s\n" (translate_to_typst_string group_name);
    fprintf outh "#set enum(numbering: \"1.\", spacing: 2em)\n";
    List.iter questions
	       ~f:(fun qst ->
		   fprintf outh "+ %s\n" (translate_to_typst_string qst.Exam_j.exam_q_statement);
                   fprintf outh "  #set enum(numbering: \"(1)\", spacing: 1em)\n";
		   List.iter qst.Exam_j.exam_q_answers
			     ~f:(fun a ->
				 fprintf outh "  + (%s) %s\n" (Bool.to_string a.Exam_j.correct) (translate_to_typst_string a.Exam_j.ans)
				);
		  );
    fprintf outh "\n"
  );
  Out_channel.close outh;

  (* Write TEST_PAPERS.typ *)
  let outh = Out_channel.create (resolve_path "TEST_PAPERS.typ") in
  fprintf outh "%s\n" typst_pre_content;
  
  List.iter papers
	    ~f:(fun paper ->
		let numQ = List.length paper.Exam_j.paper_questions in
		let ansPerLine = if numQ > 10 then 8 else numQ in
		let numLines = if numQ % ansPerLine = 0
			       then numQ / ansPerLine
			       else (numQ / ansPerLine) + 1 in

                fprintf outh "#grid(\n";
                fprintf outh "  columns: (auto, 1.5cm, auto),\n";
                fprintf outh "  align: (left, left, left),\n";
                fprintf outh "  [*#text(size: 14pt)[%d]*],\n" paper.Exam_j.paper_serial;
                fprintf outh "  [],\n";
                fprintf outh "  table(\n";
                fprintf outh "    columns: %d,\n" ansPerLine;
                fprintf outh "    align: left + horizon,\n";
                for l = 0 to numLines - 1 do
                  for i = 1 to ansPerLine do
                    let q_num = l * ansPerLine + i in
                    if q_num <= numQ then
                      fprintf outh "    [*%d:* #h(0.5cm)],\n" q_num
                    else
                      fprintf outh "    [],\n"
                  done
                done;
                fprintf outh "  )\n";
                fprintf outh ")\n\n";

                fprintf outh "#v(0.5cm)\n";
                fprintf outh "#align(left)[*Κατεύθυνση:* #box(width: 8cm, stroke: (bottom: 0.5pt))]\n";
                fprintf outh "#v(0.1cm)\n";
                fprintf outh "#align(left)[*Όνομα/Α.Μ.:* #box(width: 8cm, stroke: (bottom: 0.5pt))]\n";
                fprintf outh "#v(0.4cm)\n";
                fprintf outh "#align(center)[#text(size: 14pt, weight: \"bold\")[%s]]\n\n" (translate_to_typst_string exam.Exam_j.exam_e_name);

                fprintf outh "#set enum(numbering: \"1.\", spacing: 2em)\n";
		List.iter paper.Exam_j.paper_questions
			  ~f:(fun qst ->
			      fprintf outh "+ %s\n" (translate_to_typst_string qst.Exam_j.exam_q_statement);
			      let parent_grp = qst.Exam_j.exam_q_group in
			      let save = List.Assoc.find_exn assoc_list ~equal:(String.equal) parent_grp in
			      match save with
			      | false -> fprintf outh "  #set enum(numbering: \"(1)\", spacing: 1em)\n";
					 List.iter qst.Exam_j.exam_q_answers
						   ~f:(fun a ->
						       fprintf outh "  + %s\n" (translate_to_typst_string a.Exam_j.ans)
						      );
			      | true -> fprintf outh "  #v(0.1cm)\n";
                                        fprintf outh "  ";
					let l = List.init (List.length qst.Exam_j.exam_q_answers)
							  ~f:(fun i ->  i+1) in
					let zipl = List.zip_exn l qst.Exam_j.exam_q_answers in
					List.iter zipl
						  ~f:(fun (i,a)->
						      fprintf outh "(%d) %s #h(0.8cm) " i (translate_to_typst_string a.Exam_j.ans);
						     );
					fprintf outh "\n"
			     );
		fprintf outh "\n";
                fprintf outh "#v(1fr)\n";
                fprintf outh "#line(length: 100%%, stroke: 0.5pt)\n";
		fprintf outh "%s\n" (translate_to_typst_string exam.Exam_j.exam_e_footer);
                fprintf outh "#pagebreak()\n\n";
	       );
  Out_channel.close outh

let typstStuff ?dest_dir typ_file =
  let resolve_path path =
    match dest_dir with
    | Some dir -> Stdlib.Filename.concat dir path
    | None -> path
  in
  let pdf_file =
    let base = List.hd_exn (String.split ~on:'.' typ_file) in
    base ^ ".pdf"
  in
  let cmd =
    match dest_dir with
    | Some _ ->
        Printf.sprintf "typst compile %s %s" (resolve_path typ_file) (resolve_path pdf_file)
    | None ->
        Printf.sprintf "typst compile %s %s" typ_file pdf_file
  in
  let e = Stdlib.Sys.command cmd in
  if e <> 0 then (
    eprintf "Your typst file, %s, failed to compile." typ_file;
    exit 1
  )
