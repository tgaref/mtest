(*------------------------------- FILENAMES ------------------------------*)

open Core

let examProfileF = "examProfile.json"

let markProfileF = "markProfile.json"

let allQuestionsTex = "ALL_QUESTIONS.tex"

let testPapersTex = "TEST_PAPERS.tex"

let testPapersF = "_testPapers_"

let examF = "_exam_"

let assocF = "_assocList_"

let correctAnswersCSV = "CORRECT_ANSWERS.csv"

let correctAnsF = "_correctAns_"

let resultsCSV = "RESULTS.csv"

let latexPreampleF = "_latexPreample_"


(*--------------- FUNCTIONS FOR READING AND WRITING FILES ---------------*)

		       
let readFile file =
  let info = In_channel.with_file file ~f:(fun handle -> In_channel.input_lines handle) in
  String.concat info

let writeFile outfile data =
  let handle = Out_channel.create outfile in
  fprintf handle "%s" data;
  Out_channel.close handle


let readCSV file =
  let inh = In_channel.create file in
  let info = In_channel.input_lines inh in
  List.map info
	   ~f:(fun line ->
	       let str_list = String.split ~on:',' line in
	       List.map str_list ~f:String.strip
	      )
	   

let writeCSV file data =
    let temp = List.map data
			~f:(fun lst ->
			    String.concat ~sep:"," lst
			   ) in
    let outh = Out_channel.create file in
    List.iter temp ~f:(fun line -> fprintf outh "%s\n" line);
    Out_channel.close outh
  
