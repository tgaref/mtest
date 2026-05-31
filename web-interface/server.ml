(* server.ml - MultiTest Yume Web Interface *)

open Core

(* Helper to copy file *)
let copy_file src dst =
  try
    let content = In_channel.read_all src in
    Out_channel.write_all dst ~data:content;
    true
  with _ -> false

(* Helper to recursively create directory *)
let rec mkdir_p path =
  try
    if not (Stdlib.Sys.file_exists path && Stdlib.Sys.is_directory path) then (
      let parent = Stdlib.Filename.dirname path in
      if not (String.equal parent path) then mkdir_p parent;
      Stdlib.Sys.mkdir path 0o755
    )
  with _ -> ()

(* Helper to copy directory contents *)
let copy_directory src dst =
  try
    if Stdlib.Sys.file_exists src && Stdlib.Sys.is_directory src then (
      if not (Stdlib.Sys.file_exists dst && Stdlib.Sys.is_directory dst) then
        mkdir_p dst;
      let files = Stdlib.Sys.readdir src in
      Array.iter files ~f:(fun f ->
        let s = Stdlib.Filename.concat src f in
        let d = Stdlib.Filename.concat dst f in
        if not (Stdlib.Sys.is_directory s) then (
          let _ = copy_file s d in ()
        )
      )
    )
  with _ -> ()

(* Subprocess runner to capture stdout/stderr *)
let run_command cmd =
  let log_file = "temp_output.log" in
  let full_cmd = Printf.sprintf "%s > %s 2>&1" cmd log_file in
  let exit_code = Stdlib.Sys.command full_cmd in
  let output =
    if Stdlib.Sys.file_exists log_file then (
      let content = In_channel.read_all log_file in
      (try Stdlib.Sys.remove log_file with _ -> ());
      content
    ) else ""
  in
  (exit_code, output)

(* Helper to extract only the last non-empty line of command output (to filter out GTK/Zenity warning logs) *)
let get_last_line s =
  let lines = String.split_lines s in
  let non_empty = List.filter lines ~f:(fun l -> String.length (String.strip l) > 0) in
  match List.last non_empty with
  | Some l -> String.strip l
  | None -> ""

(* HTML Form Template *)
let render_hub ~status_alert ~console_log ~generated_files ~dest_dir ~active_tab =

  let files_badges =
    if List.is_empty generated_files then
      "<p style=\"color: var(--text-muted); font-size: 0.9rem; font-style: italic;\">No files generated yet or directory is empty.</p>"
    else
      generated_files
      |> List.map ~f:(fun f ->
           Printf.sprintf {|
             <div class="file-badge">
               <svg class="icon" width="16" height="16" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
                 <path stroke-linecap="round" stroke-linejoin="round" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
               </svg>
               %s
             </div>
           |} f)
      |> String.concat ~sep:"\n"
  in

  let output_style = if String.is_empty console_log && List.is_empty generated_files then "display: none;" else "display: block;" in

  Printf.sprintf {|
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>MultiTest - Control Hub</title>
  <meta name="description" content="Generate, mark, and manage multiple choice tests with elegance and precision.">
  <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=Outfit:wght@400;500;600;700;800&family=Fira+Code:wght@400;500&display=swap" rel="stylesheet">
  <style>
    :root {
      --bg-primary: #08080c;
      --bg-secondary: #0f0f18;
      --bg-tertiary: #171725;
      --accent: #6366f1;
      --accent-grad: linear-gradient(135deg, #6366f1 0%%, #a855f7 100%%);
      --accent-hover: #4f46e5;
      --text-main: #f3f4f6;
      --text-muted: #9ca3af;
      --border: #22223b;
      --success: #10b981;
      --error: #ef4444;
    }

    .tabs-container {
      display: flex;
      gap: 1rem;
      justify-content: center;
      margin-bottom: 1.5rem;
    }

    .tab-btn {
      background-color: var(--bg-secondary);
      border: 1px solid var(--border);
      color: var(--text-muted);
      padding: 0.75rem 1.5rem;
      border-radius: 8px;
      font-family: 'Outfit', sans-serif;
      font-weight: 600;
      font-size: 0.95rem;
      cursor: pointer;
      display: inline-flex;
      align-items: center;
      gap: 0.5rem;
      transition: all 0.2s ease;
      box-shadow: none;
    }

    .tab-btn:hover {
      color: var(--text-main);
      border-color: rgba(99, 102, 241, 0.4);
    }

    .tab-btn.active {
      background: var(--accent-grad);
      color: #fff;
      border-color: transparent;
      box-shadow: 0 4px 12px rgba(99, 102, 241, 0.3);
    }

    .tab-content {
      display: none;
      width: 100%%;
    }

    .tab-content.active {
      display: block;
    }

    .editor-group-box {
      background-color: var(--bg-tertiary);
      border: 1px solid var(--border);
      border-radius: 12px;
      margin-bottom: 1.5rem;
      overflow: hidden;
      transition: border-color 0.3s ease;
    }

    .editor-group-box:hover {
      border-color: rgba(99, 102, 241, 0.3);
    }

    .editor-group-header {
      background-color: rgba(255, 255, 255, 0.02);
      padding: 1rem 1.5rem;
      display: flex;
      justify-content: space-between;
      align-items: center;
      border-bottom: 1px solid var(--border);
      font-family: 'Outfit', sans-serif;
    }

    .editor-group-title {
      display: flex;
      align-items: center;
      gap: 0.75rem;
      font-weight: 700;
      color: var(--text-main);
    }

    .editor-group-actions {
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    .editor-group-body {
      padding: 1.5rem;
      display: flex;
      flex-direction: column;
      gap: 1.5rem;
    }

    .editor-group-body.collapsed {
      display: none;
    }

    .editor-group-btn {
      background: none;
      border: none;
      color: var(--text-muted);
      cursor: pointer;
      padding: 0.25rem 0.5rem;
      font-size: 0.85rem;
      font-weight: 600;
      display: flex;
      align-items: center;
      gap: 0.25rem;
      box-shadow: none;
      width: auto;
    }

    .editor-group-btn:hover {
      color: var(--text-main);
    }

    .editor-version-card {
      background-color: var(--bg-secondary);
      border: 1px solid var(--border);
      border-radius: 8px;
      padding: 1.25rem;
      display: flex;
      flex-direction: column;
      gap: 1rem;
      position: relative;
    }

    .editor-version-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      font-size: 0.9rem;
      font-weight: 600;
      color: var(--accent);
      border-bottom: 1px solid rgba(255, 255, 255, 0.03);
      padding-bottom: 0.4rem;
    }

    .editor-answers-container {
      display: flex;
      flex-direction: column;
      gap: 0.75rem;
      margin-top: 0.5rem;
    }

    .editor-answer-row {
      display: flex;
      align-items: center;
      gap: 0.75rem;
    }

    .editor-delete-btn {
      background-color: rgba(239, 68, 68, 0.1);
      color: var(--error);
      border: 1px solid rgba(239, 68, 68, 0.2);
      border-radius: 6px;
      padding: 0.35rem 0.65rem;
      font-size: 0.8rem;
      cursor: pointer;
      width: auto;
      box-shadow: none;
      transition: all 0.2s ease;
    }

    .editor-delete-btn:hover {
      background-color: var(--error);
      color: #fff;
    }

    * {
      box-sizing: border-box;
      margin: 0;
      padding: 0;
    }

    body {
      background-color: var(--bg-primary);
      color: var(--text-main);
      font-family: 'Inter', sans-serif;
      min-height: 100vh;
      display: flex;
      flex-direction: column;
      align-items: center;
      padding: 3rem 1rem;
      overflow-x: hidden;
    }

    h1, h2, h3 {
      font-family: 'Outfit', sans-serif;
      font-weight: 700;
    }

    .container {
      width: 100%%;
      max-width: 960px;
      display: flex;
      flex-direction: column;
      gap: 2rem;
    }

    header {
      text-align: center;
      padding-bottom: 2rem;
      position: relative;
    }

    header::after {
      content: '';
      position: absolute;
      bottom: 0;
      left: 30%%;
      width: 40%%;
      height: 2px;
      background: var(--accent-grad);
      border-radius: 2px;
    }

    header h1 {
      font-size: 2.8rem;
      background: var(--accent-grad);
      -webkit-background-clip: text;
      -webkit-text-fill-color: transparent;
      margin-bottom: 0.5rem;
      letter-spacing: -0.05em;
    }

    header p {
      color: var(--text-muted);
      font-size: 1.15rem;
      font-weight: 300;
    }

    .card {
      background-color: var(--bg-secondary);
      border: 1px solid var(--border);
      border-radius: 16px;
      padding: 2.5rem;
      box-shadow: 0 10px 30px rgba(0, 0, 0, 0.6);
      backdrop-filter: blur(10px);
      transition: border-color 0.3s ease;
    }

    .card:hover {
      border-color: rgba(99, 102, 241, 0.3);
    }

    .form-group {
      margin-bottom: 1.5rem;
      display: flex;
      flex-direction: column;
      gap: 0.5rem;
    }

    label {
      font-family: 'Outfit', sans-serif;
      font-size: 0.95rem;
      font-weight: 600;
      color: var(--text-main);
      display: flex;
      align-items: center;
      gap: 0.5rem;
    }

    .label-desc {
      font-size: 0.8rem;
      color: var(--text-muted);
      font-weight: 400;
    }

    input[type="text"], select, input[type="file"] {
      width: 100%%;
      padding: 0.85rem 1rem;
      background-color: var(--bg-tertiary);
      border: 1px solid var(--border);
      border-radius: 8px;
      color: var(--text-main);
      font-family: inherit;
      font-size: 0.95rem;
      outline: none;
      transition: border-color 0.2s ease, box-shadow 0.2s ease;
    }

    input[type="file"]::file-selector-button {
      background-image: var(--accent-grad);
      border: none;
      color: white;
      padding: 0.45rem 0.9rem;
      border-radius: 6px;
      cursor: pointer;
      font-family: inherit;
      font-weight: 600;
      font-size: 0.85rem;
      margin-right: 0.75rem;
      transition: opacity 0.2s ease;
    }
    input[type="file"]::file-selector-button:hover {
      opacity: 0.9;
    }

    input[type="text"]:focus, select:focus {
      border-color: var(--accent);
      box-shadow: 0 0 0 3px rgba(99, 102, 241, 0.2);
    }

    .row {
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 1.5rem;
    }

    @media (max-width: 768px) {
      .row {
        grid-template-columns: 1fr;
      }
    }

    .btn-container {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(180px, 1fr));
      gap: 1rem;
      margin-top: 1rem;
    }

    button {
      padding: 1rem 1.5rem;
      font-family: 'Outfit', sans-serif;
      font-weight: 600;
      font-size: 1rem;
      border: none;
      border-radius: 8px;
      cursor: pointer;
      color: #fff;
      display: flex;
      align-items: center;
      justify-content: center;
      gap: 0.5rem;
      transition: all 0.2s ease;
      box-shadow: 0 4px 12px rgba(0, 0, 0, 0.3);
    }

    .btn-create {
      background: var(--accent-grad);
    }

    .btn-create:hover {
      filter: brightness(1.15);
      transform: translateY(-2px);
    }

    .btn-profile {
      background-color: #2563eb;
    }

    .btn-profile:hover {
      background-color: #1d4ed8;
      transform: translateY(-2px);
    }

    .btn-mark {
      background-color: #059669;
    }

    .btn-mark:hover {
      background-color: #047857;
      transform: translateY(-2px);
    }

    .btn-backup {
      background-color: #d97706;
    }

    .btn-backup:hover {
      background-color: #b45309;
      transform: translateY(-2px);
    }

    .terminal-card {
      background-color: #040406;
      border: 1px solid var(--border);
      border-radius: 12px;
      padding: 1.5rem;
      font-family: 'Fira Code', monospace;
      font-size: 0.9rem;
      line-height: 1.6;
      overflow-x: auto;
      max-height: 400px;
      color: #38bdf8;
      box-shadow: inset 0 0 20px rgba(0,0,0,0.9);
      position: relative;
    }

    .terminal-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-bottom: 1rem;
      padding-bottom: 0.5rem;
      border-bottom: 1px solid #141424;
    }

    .terminal-dots {
      display: flex;
      gap: 0.4rem;
    }

    .dot {
      width: 12px;
      height: 12px;
      border-radius: 50%%;
    }

    .dot-red { background-color: #ef4444; }
    .dot-yellow { background-color: #f59e0b; }
    .dot-green { background-color: #10b981; }

    .terminal-title {
      font-size: 0.8rem;
      color: var(--text-muted);
      font-weight: 500;
    }

    .alert {
      padding: 1.25rem;
      border-radius: 8px;
      margin-bottom: 1.5rem;
      font-weight: 600;
      display: flex;
      align-items: center;
      gap: 0.75rem;
      border: 1px solid transparent;
    }

    .alert-success {
      background-color: rgba(16, 185, 129, 0.1);
      border-color: rgba(16, 185, 129, 0.3);
      color: #34d399;
    }

    .alert-error {
      background-color: rgba(239, 68, 68, 0.1);
      border-color: rgba(239, 68, 68, 0.3);
      color: #f87171;
    }

    .file-grid {
      display: grid;
      grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
      gap: 1rem;
      margin-top: 1rem;
    }

    .file-badge {
      background-color: var(--bg-tertiary);
      border: 1px solid var(--border);
      padding: 0.85rem 1rem;
      border-radius: 8px;
      display: flex;
      align-items: center;
      gap: 0.65rem;
      font-size: 0.85rem;
      font-weight: 500;
      color: var(--text-main);
      word-break: break-all;
    }

    .file-badge .icon {
      color: var(--success);
      flex-shrink: 0;
    }

    .output-section h3 {
      font-size: 1.25rem;
      margin-bottom: 0.75rem;
      color: var(--text-main);
    }
  </style>
</head>
<body>
  <div class="container">
    <header>
      <h1>MultiTest Hub</h1>
      <p>Interactive Control Hub for Exam Generation & Grading</p>
    </header>

    <form action="/run" method="POST" enctype="multipart/form-data" id="main-form">
      <!-- Hidden input for the active tab -->
      <input type="hidden" name="active_tab" id="active-tab" value="%s">

      <!-- Top-Level Exam Directory Selector -->
      <div class="card" style="padding: 1.5rem; margin-bottom: 2rem; background-color: var(--bg-secondary); border: 1px solid var(--border);">
        <div class="form-group" style="margin-bottom: 0;">
          <label for="exam-dir" style="font-family: 'Outfit'; font-weight: 600; font-size: 1.1rem; color: var(--accent); margin-bottom: 0.5rem; display: block;">
            Exam Destination Directory
          </label>
          <div style="display: flex; gap: 0.75rem; align-items: center;">
            <input type="text" name="dest_dir" id="exam-dir" value="%s" placeholder="e.g. exam_backup" style="flex: 1; padding: 0.85rem 1rem; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main); font-family: inherit;">
            <button type="button" onclick="browseFile('exam-dir', 'directory', 'Select Exam Destination Directory')" style="white-space: nowrap; padding: 0.85rem 1.25rem; width: auto; background-image: var(--accent-grad); font-size: 0.95rem; border-radius: 8px; font-weight: 500;">Browse</button>
          </div>
        </div>
      </div>

      <!-- Tabs Navigation -->
      <div class="tabs-container">
        <button type="button" class="tab-btn" onclick="switchTab('questions-editor-tab', this)">
          <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"/></svg>
          Questions Editor
        </button>
        <button type="button" class="tab-btn" onclick="switchTab('exam-profile-editor-tab', this)">
          <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"/></svg>
          Exam Profile Editor
        </button>
        <button type="button" class="tab-btn" onclick="switchTab('mark-profile-editor-tab', this)">
          <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4"/></svg>
          Mark Profile Editor
        </button>
        <button type="button" class="tab-btn" onclick="switchTab('create-exam-tab', this)">
          <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 4v16m8-8H4"/></svg>
          Create Exam
        </button>
        <button type="button" class="tab-btn" onclick="switchTab('mark-exam-tab', this)">
          <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M9 12l2 2 4-4M7.835 4.697a3.42 3.42 0 001.946-.806 3.42 3.42 0 014.438 0 3.42 3.42 0 001.946.806 3.42 3.42 0 013.138 3.138 3.42 3.42 0 00.806 1.946 3.42 3.42 0 010 4.438 3.42 3.42 0 00-.806 1.946 3.42 3.42 0 01-3.138 3.138 3.42 3.42 0 00-1.946.806 3.42 3.42 0 01-4.438 0 3.42 3.42 0 00-1.946-.806 3.42 3.42 0 01-3.138-3.138 3.42 3.42 0 00-.806-1.946 3.42 3.42 0 010-4.438 3.42 3.42 0 00.806-1.946 3.42 3.42 0 013.138-3.138z"/></svg>
          Mark Exam
        </button>
      </div>

      <!-- Tab 1: Questions Editor -->
      <div id="questions-editor-tab" class="tab-content">
        <div class="card" style="padding: 2.5rem; margin-bottom: 2rem;">
          <h2 style="font-family: 'Outfit'; margin-bottom: 1.5rem; font-size: 1.8rem; display: flex; align-items: center; gap: 0.5rem;">
            <svg width="24" height="24" fill="none" stroke="var(--accent)" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M11 5H6a2 2 0 00-2 2v11a2 2 0 002 2h11a2 2 0 002-2v-5m-1.414-9.414a2 2 0 112.828 2.828L11.828 15H9v-2.828l8.586-8.586z"/></svg>
            Questions Database Editor
          </h2>
          
          <div style="display: flex; gap: 1rem; align-items: flex-end; flex-wrap: wrap; margin-bottom: 2rem; background-color: var(--bg-tertiary); padding: 1.25rem; border-radius: 12px; border: 1px solid var(--border);">
            <div style="flex: 1; min-width: 200px; display: flex; flex-direction: column; gap: 0.5rem;">
              <label for="editor-questions-path" style="font-size: 0.85rem; font-weight: 500;">Load Existing Questions JSON</label>
              <div style="display: flex; gap: 0.5rem; align-items: center;">
                <input type="text" id="editor-questions-path" placeholder="e.g. example/questions.json" style="flex: 1; padding: 0.85rem 1rem; background-color: var(--bg-secondary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main);">
                <button type="button" onclick="browseAndLoadQuestions()" style="white-space: nowrap; padding: 0.85rem 1.2rem; width: auto; background-image: var(--accent-grad); font-size: 0.9rem; border-radius: 8px;">Browse</button>
              </div>
            </div>
            <div style="display: flex; gap: 0.75rem; align-items: flex-end;">
              <button type="button" class="btn-create" onclick="createNewQuestionsDatabase()" style="padding: 0.85rem 1.25rem; font-size: 0.9rem; height: auto; width: auto; box-shadow: none; margin: 0; background-image: none; background-color: var(--bg-secondary); border: 1px solid var(--border);">
                New Empty
              </button>
            </div>
          </div>

          <!-- Editor Workspace -->
          <div id="editor-workspace" style="display: none;">
            <div class="row" style="margin-bottom: 2rem;">
              <div class="form-group" style="flex: 1;">
                <label for="editor-exam-name">
                  Exam Name
                </label>
                <input type="text" id="editor-exam-name" placeholder="e.g. COMPILER ARCHITECTURE" style="padding: 0.85rem 1rem; width: 100%%; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main);">
              </div>
              <div class="form-group" style="flex: 1;">
                <label for="editor-exam-footer">
                  Footer
                </label>
                <input type="text" id="editor-exam-footer" placeholder="e.g. Page %%d" style="padding: 0.85rem 1rem; width: 100%%; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main);">
              </div>
            </div>

            <div style="display: flex; align-items: center; justify-content: space-between; margin-bottom: 1.5rem;">
              <h3 style="font-family: 'Outfit'; margin: 0;">Questions & Groups <span id="question-count-badge" class="badge" style="background-color: var(--accent); color: white; padding: 0.25rem 0.6rem; border-radius: 20px; font-size: 0.8rem; margin-left: 0.5rem; font-family: 'Outfit';">0 questions</span></h3>
              <button type="button" class="btn-create" onclick="addGroup()" style="padding: 0.6rem 1.25rem; font-size: 0.85rem; width: auto; border-radius: 8px; background-image: var(--accent-grad);">
                + Add Group
              </button>
            </div>

            <div id="editor-questions-container" style="display: flex; flex-direction: column; gap: 2rem; margin-bottom: 2.5rem;">
              <!-- Question groups rendered dynamically -->
            </div>

            <div style="display: flex; justify-content: flex-end; gap: 1rem;">
              <button type="button" class="btn-backup" onclick="saveQuestionsToDisk()" style="padding: 1rem 2rem; width: auto; border-radius: 8px;">
                <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M8 7H5a2 2 0 00-2 2v9a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-3m-1 4l-3 3m0 0l-3-3m3 3V4"/></svg>
                Save
              </button>
            </div>
          </div>
        </div>
      </div> <!-- Close Questions Editor Tab -->

      <!-- Tab 2: Exam Profile Editor -->
      <div id="exam-profile-editor-tab" class="tab-content">
        <div class="card" style="padding: 2.5rem; margin-bottom: 2rem;">
          <h2 style="font-family: 'Outfit'; margin-bottom: 1.5rem; font-size: 1.8rem; display: flex; align-items: center; gap: 0.5rem;">
            <svg width="24" height="24" fill="none" stroke="var(--accent)" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"/></svg>
            Exam Profile Editor
          </h2>

          <div style="display: flex; gap: 1rem; align-items: flex-end; flex-wrap: wrap; margin-bottom: 2rem; background-color: var(--bg-tertiary); padding: 1.25rem; border-radius: 12px; border: 1px solid var(--border);">
            <div style="flex: 1; min-width: 200px; display: flex; flex-direction: column; gap: 0.5rem;">
              <label for="profile-questions-path" style="font-size: 0.85rem; font-weight: 500;">Load Questions JSON to Base Profile On</label>
              <div style="display: flex; gap: 0.5rem; align-items: center;">
                <input type="text" id="profile-questions-path" placeholder="e.g. example/questions.json" style="flex: 1; padding: 0.85rem 1rem; background-color: var(--bg-secondary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main);">
                <button type="button" onclick="browseAndLoadProfileQuestions()" style="white-space: nowrap; padding: 0.85rem 1.2rem; width: auto; background-image: var(--accent-grad); font-size: 0.9rem; border-radius: 8px;">Browse</button>
              </div>
            </div>
          </div>

          <!-- Profile Workspace -->
          <div id="profile-workspace" style="display: none;">
            <div class="row" style="margin-bottom: 2rem;">
              <div class="form-group" style="flex: 1;">
                <label for="profile-total-num">
                  Number of Tests to Create
                </label>
                <input type="number" id="profile-total-num" value="10" min="1" style="padding: 0.85rem 1rem; width: 100%%; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main);">
              </div>
              <div class="form-group" style="flex: 1;">
                <label for="profile-seed">
                  Random Seed
                </label>
                <input type="text" id="profile-seed" value="1a2b3c4d" style="padding: 0.85rem 1rem; width: 100%%; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main);">
              </div>
            </div>

            <h3 style="font-family: 'Outfit'; margin-bottom: 1rem;">Group Pick Rules</h3>

            <div style="overflow-x: auto; border: 1px solid var(--border); border-radius: 12px; margin-bottom: 2rem; background-color: rgba(255,255,255,0.01);">
              <table style="width: 100%%; border-collapse: collapse; font-size: 0.95rem; text-align: left;">
                <thead>
                  <tr style="border-bottom: 1px solid var(--border); background-color: rgba(255,255,255,0.03);">
                    <th style="padding: 1rem 1.5rem; font-family: 'Outfit'; font-weight: 700; color: var(--accent); width: 40%%;">Group</th>
                    <th style="padding: 1rem; font-family: 'Outfit'; font-weight: 700; color: var(--accent); width: 30%%;">Questions to Pick</th>
                    <th style="padding: 1rem 1.5rem; font-family: 'Outfit'; font-weight: 700; color: var(--accent); width: 30%%;">Save Space?</th>
                  </tr>
                </thead>
                <tbody id="profile-groups-table-body">
                  <!-- Dynamic group rows -->
                </tbody>
              </table>
            </div>

            <div style="display: flex; justify-content: flex-end; gap: 1rem;">
              <button type="button" class="btn-create" onclick="saveExamProfileToDisk()" style="padding: 1rem 2rem; width: auto; border-radius: 8px;">
                <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M8 7H5a2 2 0 00-2 2v9a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-3m-1 4l-3 3m0 0l-3-3m3 3V4"/></svg>
                Save
              </button>
            </div>
          </div>
        </div>
      </div> <!-- Close Exam Profile Editor Tab -->

      <!-- Tab 3: Mark Profile Editor -->
      <div id="mark-profile-editor-tab" class="tab-content">
        <div class="card" style="padding: 2.5rem; margin-bottom: 2rem;">
          <h2 style="font-family: 'Outfit'; margin-bottom: 1.5rem; font-size: 1.8rem; display: flex; align-items: center; gap: 0.5rem;">
            <svg width="24" height="24" fill="none" stroke="var(--accent)" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4"/></svg>
            Mark Profile Editor
          </h2>

          <div style="display: flex; gap: 1rem; align-items: flex-end; flex-wrap: wrap; margin-bottom: 2rem; background-color: var(--bg-tertiary); padding: 1.25rem; border-radius: 12px; border: 1px solid var(--border);">
            <div style="flex: 1; min-width: 200px; display: flex; flex-direction: column; gap: 0.5rem;">
              <label for="mark-exam-profile-path" style="font-size: 0.85rem; font-weight: 500;">Load Exam Profile JSON</label>
              <div style="display: flex; gap: 0.5rem; align-items: center;">
                <input type="text" id="mark-exam-profile-path" placeholder="e.g. examProfile.json" style="flex: 1; padding: 0.85rem 1rem; background-color: var(--bg-secondary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main);">
                <button type="button" onclick="browseAndLoadMarkExamProfile()" style="white-space: nowrap; padding: 0.85rem 1.2rem; width: auto; background-image: var(--accent-grad); font-size: 0.9rem; border-radius: 8px;">Browse</button>
              </div>
            </div>
          </div>

          <!-- Mark Profile Workspace -->
          <div id="mark-profile-workspace" style="display: none;">
            <h3 style="font-family: 'Outfit'; margin-bottom: 1rem;">Group Marking Rules</h3>

            <div style="overflow-x: auto; border: 1px solid var(--border); border-radius: 12px; margin-bottom: 2rem; background-color: rgba(255,255,255,0.01);">
              <table style="width: 100%%; border-collapse: collapse; font-size: 0.95rem; text-align: left;">
                <thead>
                  <tr style="border-bottom: 1px solid var(--border); background-color: rgba(255,255,255,0.03);">
                    <th style="padding: 1rem 1.5rem; font-family: 'Outfit'; font-weight: 700; color: var(--accent); width: 40%%;">Group</th>
                    <th style="padding: 1rem; font-family: 'Outfit'; font-weight: 700; color: var(--accent); width: 30%%;">Positive Mark (+)</th>
                    <th style="padding: 1rem; font-family: 'Outfit'; font-weight: 700; color: var(--accent); width: 30%%;">Negative Mark (-)</th>
                  </tr>
                </thead>
                <tbody id="mark-profile-groups-table-body">
                  <!-- Dynamic group rows -->
                </tbody>
              </table>
            </div>

            <div style="display: flex; justify-content: flex-end; gap: 1rem;">
              <button type="button" class="btn-create" onclick="saveMarkProfileToDisk()" style="padding: 1rem 2rem; width: auto; border-radius: 8px;">
                <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M8 7H5a2 2 0 00-2 2v9a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-3m-1 4l-3 3m0 0l-3-3m3 3V4"/></svg>
                Save
              </button>
            </div>
          </div>
        </div>
      </div> <!-- Close Mark Profile Editor Tab -->

      <!-- Tab 4: Create Exam -->
      <div id="create-exam-tab" class="tab-content">
        <div class="card" style="padding: 2.5rem; margin-bottom: 2rem;">
          <h2 style="font-family: 'Outfit'; margin-bottom: 1.5rem; font-size: 1.8rem; display: flex; align-items: center; gap: 0.5rem;">
            <svg width="24" height="24" fill="none" stroke="var(--accent)" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 4v16m8-8H4"/></svg>
            Create Exam
          </h2>
          <p class="label-desc" style="margin-bottom: 2rem;">Select your questions database and exam profile config to generate test papers.</p>
          
          <div class="row">
            <div class="form-group">
              <label for="create-questions-path">
                Questions JSON File
              </label>
              <div style="display: flex; gap: 0.5rem; align-items: center;">
                <input type="text" name="questions_path" id="create-questions-path" placeholder="e.g. example/questions.json" style="flex: 1;">
                <button type="button" onclick="browseFile('create-questions-path', '*.json', 'Select Questions JSON')" style="white-space: nowrap; padding: 0.85rem 1.2rem; width: auto; background-image: var(--accent-grad); font-size: 0.9rem; border-radius: 8px;">Browse</button>
              </div>
            </div>

            <div class="form-group">
              <label for="create-exam-profile-path">
                Exam Profile JSON File
              </label>
              <div style="display: flex; gap: 0.5rem; align-items: center;">
                <input type="text" name="exam_profile_path" id="create-exam-profile-path" placeholder="e.g. examProfile.json" style="flex: 1;">
                <button type="button" onclick="browseFile('create-exam-profile-path', '*.json', 'Select Exam Profile JSON')" style="white-space: nowrap; padding: 0.85rem 1.2rem; width: auto; background-image: var(--accent-grad); font-size: 0.9rem; border-radius: 8px;">Browse</button>
              </div>
            </div>
          </div>

          <div class="btn-container" style="justify-content: flex-end; margin-top: 1.5rem;">
            <button type="submit" name="action" value="create" class="btn-create" id="btn-create" style="width: auto; padding: 1rem 2rem;">
              <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 4v16m8-8H4"/></svg>
              Generate Papers
            </button>
          </div>
        </div>
      </div> <!-- Close Create Exam Tab -->

      <!-- Tab 5: Mark Exam -->
      <div id="mark-exam-tab" class="tab-content">
        <div class="card" style="padding: 2.5rem; margin-bottom: 2rem;">
          <h2 style="font-family: 'Outfit'; margin-bottom: 1.5rem; font-size: 1.8rem; display: flex; align-items: center; gap: 0.5rem;">
            <svg width="24" height="24" fill="none" stroke="var(--accent)" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4"/></svg>
            Mark Exam
          </h2>
          <p class="label-desc" style="margin-bottom: 2rem;">Select the given answers CSV and your mark profile JSON to grade student papers.</p>
          
          <div class="row">
            <div class="form-group">
              <label for="mark-given-answers-path">
                Given Answers CSV File
              </label>
              <div style="display: flex; gap: 0.5rem; align-items: center;">
                <input type="text" name="given_answers_path" id="mark-given-answers-path" placeholder="e.g. givenAnswers.csv" style="flex: 1;">
                <button type="button" onclick="browseFile('mark-given-answers-path', '*.csv', 'Select Given Answers CSV')" style="white-space: nowrap; padding: 0.85rem 1.2rem; width: auto; background-image: var(--accent-grad); font-size: 0.9rem; border-radius: 8px;">Browse</button>
              </div>
            </div>

            <div class="form-group">
              <label for="mark-profile-path">
                Mark Profile JSON File
              </label>
              <div style="display: flex; gap: 0.5rem; align-items: center;">
                <input type="text" name="mark_profile_path" id="mark-profile-path" placeholder="e.g. markProfile.json" style="flex: 1;">
                <button type="button" onclick="browseFile('mark-profile-path', '*.json', 'Select Mark Profile JSON')" style="white-space: nowrap; padding: 0.85rem 1.2rem; width: auto; background-image: var(--accent-grad); font-size: 0.9rem; border-radius: 8px;">Browse</button>
              </div>
            </div>
          </div>

          <div class="btn-container" style="justify-content: flex-end; margin-top: 1.5rem;">
            <button type="submit" name="action" value="mark" class="btn-mark" id="btn-mark" style="width: auto; padding: 1rem 2rem;">
              <svg width="18" height="18" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M9 5H7a2 2 0 00-2 2v12a2 2 0 002 2h10a2 2 0 002-2V7a2 2 0 00-2-2h-2M9 5a2 2 0 002 2h2a2 2 0 002-2M9 5a2 2 0 012-2h2a2 2 0 012 2m-6 9l2 2 4-4"/></svg>
              Grade Papers
            </button>
          </div>
        </div>
      </div> <!-- Close Mark Exam Tab -->

    </form>

    <!-- Output Section -->
    <div class="output-section" style="%s" id="output-section">
      %s

      <div class="card" style="margin-top: 1.5rem; padding: 2rem;">
        <h3 style="font-family: 'Outfit'; margin-bottom: 0.5rem;">Target Directory Files</h3>
        <p class="label-desc" style="margin-bottom: 1rem;">Verified files currently stored in: <code>%s</code></p>
        <div class="file-grid">
          %s
        </div>
      </div>

      <div style="margin-top: 1.5rem;">
        <div class="terminal-card">
          <div class="terminal-header">
            <div class="terminal-dots">
              <div class="dot dot-red"></div>
              <div class="dot dot-yellow"></div>
              <div class="dot dot-green"></div>
            </div>
            <div class="terminal-title">bash - mtest.exe console output</div>
            <div style="width: 50px;"></div>
          </div>
          <pre style="white-space: pre-wrap; font-family: inherit;">%s</pre>
        </div>
      </div>
    </div>

  </div> <!-- Close Container -->
  <!-- Directory picker modal removed in favor of standard webkitdirectory folder upload selector -->

  <script>
    let currentPath = ".";
    let editorQuestions = [];
    let loadedFilePath = "";

    let collapsedGroups = {};
    let profileGroups = [];

    function switchTab(tabId, btn) {
      document.querySelectorAll(".tab-content").forEach(el => el.classList.remove("active"));
      document.querySelectorAll(".tab-btn").forEach(el => el.classList.remove("active"));
      
      document.getElementById(tabId).classList.add("active");
      btn.classList.add("active");
      
      const hiddenInput = document.getElementById("active-tab");
      if (hiddenInput) {
        hiddenInput.value = tabId;
      }
    }

    function browseAndLoadQuestions() {
      browseFile('editor-questions-path', '*.json', 'Select Questions JSON');
      // Wait briefly for the picker to finish, then auto-load
      const checkInterval = setInterval(() => {
        const path = document.getElementById('editor-questions-path').value;
        if (path) {
          clearInterval(checkInterval);
          loadQuestionsFromPath();
        }
      }, 500);
      // Stop checking after 60 seconds
      setTimeout(() => clearInterval(checkInterval), 60000);
    }

    function loadQuestionsFromPath() {
      const path = document.getElementById('editor-questions-path').value.trim();
      if (!path) {
        alert('Please enter or browse for a questions JSON file path.');
        return;
      }
      
      fetch('/editor/load?file=' + encodeURIComponent(path))
        .then(res => res.json())
        .then(json => {
          if (json.status === 'error') {
            alert('Error loading file: ' + json.message);
            return;
          }
          if (!json.questions || !Array.isArray(json.questions)) {
            alert("Invalid questions database: missing 'questions' list.");
            return;
          }
          
          loadedFilePath = path;
          document.getElementById('editor-exam-name').value = json.name || '';
          document.getElementById('editor-exam-footer').value = json.footer || '';
          editorQuestions = json.questions;
          collapsedGroups = {};
          renderEditorQuestions();
          document.getElementById('editor-workspace').style.display = 'block';
          showTemporaryEditorAlert('Successfully loaded ' + editorQuestions.length + ' questions from ' + path + '!', 'success');
        })
        .catch(err => alert('Error loading questions: ' + err));
    }

    function browseAndLoadProfileQuestions() {
      browseFile('profile-questions-path', '*.json', 'Select Questions JSON');
      const checkInterval = setInterval(() => {
        const path = document.getElementById('profile-questions-path').value;
        if (path) {
          clearInterval(checkInterval);
          loadProfileQuestionsFromPath();
        }
      }, 500);
      setTimeout(() => clearInterval(checkInterval), 60000);
    }

    function loadProfileQuestionsFromPath() {
      const path = document.getElementById('profile-questions-path').value.trim();
      if (!path) {
        alert('Please enter or browse for a questions JSON file path.');
        return;
      }
      
      fetch('/editor/load?file=' + encodeURIComponent(path))
        .then(res => res.json())
        .then(json => {
          if (json.status === 'error') {
            alert('Error loading file: ' + json.message);
            return;
          }
          if (!json.questions || !Array.isArray(json.questions)) {
            alert("Invalid questions database: missing 'questions' list.");
            return;
          }
          
          const uniqueGroups = new Set();
          json.questions.forEach(q => {
            if (q.group) uniqueGroups.add(q.group.trim());
          });
          
          profileGroups = Array.from(uniqueGroups).sort();
          renderProfileWorkspace();
          document.getElementById('profile-workspace').style.display = 'block';
          showTemporaryProfileAlert('Successfully parsed ' + profileGroups.length + ' unique question groups from ' + path + '!', 'success');
        })
        .catch(err => alert('Error loading questions: ' + err));
    }


    function toggleGroup(gName) {
      collapsedGroups[gName] = !collapsedGroups[gName];
      renderEditorQuestions();
    }

    function deleteGroup(gName) {
      if (confirm(`Are you sure you want to delete Group "${gName}" and all its questions?`)) {
        editorQuestions = editorQuestions.filter(q => q.group !== gName);
        renderEditorQuestions();
      }
    }

    function updateGroupName(oldName, newName) {
      newName = newName.trim();
      if (!newName || newName === oldName) return;
      
      editorQuestions.forEach(q => {
        if (q.group === oldName) {
          q.group = newName;
        }
      });
      
      if (collapsedGroups[oldName] !== undefined) {
        collapsedGroups[newName] = collapsedGroups[oldName];
        delete collapsedGroups[oldName];
      }
      
      renderEditorQuestions();
    }

    function addQuestionToGroup(gName) {
      editorQuestions.push({
        group: gName,
        statement: "",
        answers: [
          { ans: "", correct: true },
          { ans: "", correct: false },
          { ans: "", correct: false },
          { ans: "", correct: false }
        ]
      });
      renderEditorQuestions();
    }

    function addGroup() {
      let gName = prompt("Enter the name of the new group (e.g. Q3):");
      if (!gName) return;
      gName = gName.trim();
      if (!gName) return;
      
      const exists = editorQuestions.some(q => q.group === gName);
      if (exists) {
        alert("A group with this name already exists.");
        return;
      }
      
      addQuestionToGroup(gName);
    }

    // Unused picker function removed in favor of standard HTML5 file upload inputs

    function startNewEmptyQuestions() {
      document.getElementById("editor-exam-name").value = "NEW EXAM";
      document.getElementById("editor-exam-footer").value = "All rights reserved";
      editorQuestions = [];
      loadedFilePath = "";
      collapsedGroups = {};
      renderEditorQuestions();
      document.getElementById("editor-workspace").style.display = "block";
      showTemporaryEditorAlert("Created a new empty questions database!", "success");
    }

    function loadQuestionsForEditing(customPath = "") {
      // Unused loading function removed in favor of instant client-side FileReader parsing
    }

    function renderEditorQuestions() {
      const container = document.getElementById("editor-questions-container");
      container.innerHTML = "";
      
      document.getElementById("question-count-badge").textContent = editorQuestions.length + " question" + (editorQuestions.length === 1 ? "" : "s");
      
      const groups = {};
      const groupOrder = [];
      
      editorQuestions.forEach((q, index) => {
        let gName = (q.group || "No Group").trim();
        if (!groups[gName]) {
          groups[gName] = [];
          groupOrder.push(gName);
        }
        groups[gName].push({ question: q, originalIndex: index });
      });

      if (groupOrder.length === 0) {
        container.innerHTML = `<p style="color: var(--text-muted); font-style: italic; text-align: center; margin: 2rem 0;">No question groups yet. Click "+ Add Group" or "Create New" to start.</p>`;
        return;
      }
      
      groupOrder.forEach(gName => {
        const isCollapsed = !!collapsedGroups[gName];
        const gBox = document.createElement("div");
        gBox.className = "editor-group-box";
        
        let versionsHTML = "";
        const qList = groups[gName];
        
        qList.forEach((item, vIndex) => {
          const q = item.question;
          const origIdx = item.originalIndex;
          
          let answersHTML = "";
          q.answers = q.answers || [];
          q.answers.forEach((ans, ansIndex) => {
            const isCorrect = ans.correct ? "checked" : "";
            answersHTML += `
              <div class="editor-answer-row">
                <input type="checkbox" class="editor-answer-correct" ${isCorrect} style="width: auto; cursor: pointer;" onchange="updateCorrect(${origIdx}, ${ansIndex}, this.checked)">
                <input type="text" class="editor-answer-text" value="${escapeHtml(ans.ans)}" placeholder="Answer option" style="flex: 1; padding: 0.5rem 0.75rem; font-size: 0.9rem;" oninput="updateAnswer(${origIdx}, ${ansIndex}, this.value)">
                <button type="button" class="editor-delete-btn" onclick="deleteAnswerOption(${origIdx}, ${ansIndex})">
                  Remove
                </button>
              </div>
            `;
          });
          
          versionsHTML += `
            <div class="editor-version-card" style="margin-bottom: 1rem;">
              <div class="editor-version-header">
                <span>Version #${vIndex + 1}</span>
                <button type="button" class="editor-delete-btn" style="padding: 0.3rem 0.6rem; font-size: 0.75rem;" onclick="deleteQuestion(${origIdx})">
                  Delete Version
                </button>
              </div>
              <div class="form-group" style="margin-bottom: 0;">
                <label style="font-size: 0.85rem;">Question Statement</label>
                <textarea placeholder="Type question statement here (LaTeX like $2^n$ is supported)..." style="width: 100%%; min-height: 60px; padding: 0.5rem 0.75rem; font-size: 0.9rem; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 8px; color: var(--text-main); font-family: inherit; outline: none; resize: vertical; transition: border-color 0.2s ease;" oninput="updateStatement(${origIdx}, this.value)">${escapeHtml(q.statement || '')}</textarea>
              </div>
              <div style="font-weight: 600; font-size: 0.85rem; font-family: 'Outfit'; margin-top: 0.5rem; color: var(--text-muted); display: flex; justify-content: space-between; align-items: center;">
                <span>Answer Options</span>
                <button type="button" onclick="addAnswerOption(${origIdx})" style="background: none; border: none; color: var(--accent); font-weight: 600; font-size: 0.8rem; cursor: pointer; display: flex; align-items: center; gap: 0.25rem; box-shadow: none; width: auto; padding: 0;">
                  + Add Option
                </button>
              </div>
              <div class="editor-answers-container">
                ${answersHTML}
              </div>
            </div>
          `;
        });
        
        gBox.innerHTML = `
          <div class="editor-group-header">
            <div class="editor-group-title">
              <svg width="18" height="18" fill="none" stroke="var(--accent)" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M3 7v10a2 2 0 002 2h14a2 2 0 002-2V9a2 2 0 00-2-2h-6l-2-2H5a2 2 0 00-2 2z"/></svg>
              <span>Group Name:</span>
              <input type="text" value="${escapeHtml(gName)}" style="padding: 0.35rem 0.65rem; font-size: 0.9rem; width: 100px; font-weight: 700; font-family: inherit; border-radius: 6px; border: 1px solid var(--border); background-color: var(--bg-tertiary); color: var(--text-main);" onchange="updateGroupName('${escapeHtml(gName)}', this.value)">
              <span style="font-size: 0.8rem; color: var(--text-muted); font-weight: 400; margin-left: 0.5rem;">(${qList.length} version${qList.length === 1 ? "" : "s"})</span>
            </div>
            <div class="editor-group-actions">
              <button type="button" class="editor-group-btn" onclick="toggleGroup('${escapeHtml(gName)}')">
                ${isCollapsed ? "Expand &darr;" : "Collapse &uarr;"}
              </button>
              <button type="button" class="editor-delete-btn" style="padding: 0.4rem 0.8rem;" onclick="deleteGroup('${escapeHtml(gName)}')">
                Delete Group
              </button>
            </div>
          </div>
          <div class="editor-group-body ${isCollapsed ? "collapsed" : ""}" style="padding: 1.25rem;">
            ${versionsHTML}
            <div style="display: flex; justify-content: center; margin-top: 0.5rem;">
              <button type="button" class="btn-profile" style="background-color: rgba(99, 102, 241, 0.05); border: 1px dashed rgba(99, 102, 241, 0.3); padding: 0.6rem 1.25rem; font-size: 0.85rem; width: auto; box-shadow: none; margin: 0;" onclick="addQuestionToGroup('${escapeHtml(gName)}')">
                + Add Version to Group
              </button>
            </div>
          </div>
        `;
        container.appendChild(gBox);
      });
    }

    function updateStatement(idx, value) {
      if (editorQuestions[idx]) {
        editorQuestions[idx].statement = value;
      }
    }

    function updateAnswer(idx, ansIdx, value) {
      if (editorQuestions[idx] && editorQuestions[idx].answers && editorQuestions[idx].answers[ansIdx]) {
        editorQuestions[idx].answers[ansIdx].ans = value;
      }
    }

    function updateCorrect(idx, ansIdx, checked) {
      if (editorQuestions[idx] && editorQuestions[idx].answers && editorQuestions[idx].answers[ansIdx]) {
        editorQuestions[idx].answers[ansIdx].correct = checked;
      }
    }

    function addQuestionToEditor() {
      let nextGroup = "Q1";
      if (editorQuestions.length > 0) {
        let lastGroup = editorQuestions[editorQuestions.length - 1].group;
        if (lastGroup && lastGroup.startsWith("Q")) {
          let num = parseInt(lastGroup.substring(1));
          if (!isNaN(num)) {
            nextGroup = "Q" + (num + 1);
          } else {
            nextGroup = lastGroup;
          }
        } else if (lastGroup) {
          nextGroup = lastGroup;
        }
      }
      
      editorQuestions.push({
        group: nextGroup,
        statement: "",
        answers: [
          { ans: "", correct: true },
          { ans: "", correct: false },
          { ans: "", correct: false },
          { ans: "", correct: false }
        ]
      });
      renderEditorQuestions();
    }

    function deleteQuestion(origIdx) {
      if (confirm("Are you sure you want to delete this question version?")) {
        editorQuestions.splice(origIdx, 1);
        renderEditorQuestions();
      }
    }

    function addAnswerOption(origIdx) {
      editorQuestions[origIdx].answers.push({ ans: "", correct: false });
      renderEditorQuestions();
    }

    function deleteAnswerOption(origIdx, ansIdx) {
      editorQuestions[origIdx].answers.splice(ansIdx, 1);
      renderEditorQuestions();
    }

    function saveQuestionsToDisk() {
      const examDir = document.getElementById("exam-dir").value.trim();
      if (!examDir) {
        alert("Please select or enter an Exam Destination Directory at the top of the page first!");
        return;
      }

      if (editorQuestions.length === 0) {
        alert("Please add at least one question before saving.");
        return;
      }
      
      let baseName = "my_questions.json";
      if (loadedFilePath) {
        baseName = loadedFilePath.split('/').pop();
      }
      let defaultPath = examDir + "/" + baseName;
      
      fetch("/pick-save-file?default=" + encodeURIComponent(defaultPath) + "&title=" + encodeURIComponent("Save Questions JSON File"))
        .then(res => res.json())
        .then(pickerData => {
          if (pickerData.status !== "ok" || !pickerData.path) {
            console.log("Save picker cancelled or failed");
            return;
          }
          
          const filename = pickerData.path;
          const payload = {
            name: document.getElementById("editor-exam-name").value.trim(),
            footer: document.getElementById("editor-exam-footer").value.trim(),
            questions: editorQuestions
          };
          
          fetch("/editor/save", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ filename: filename, data: payload })
          })
          .then(res => res.json())
          .then(data => {
            if (data.status === "ok") {
              showTemporaryEditorAlert("File saved successfully! Location: " + data.filename, "success");
              loadedFilePath = data.filename;
              
              const cleanName = data.filename;
              const inputEl = document.getElementById("questions-select");
              if (inputEl) {
                inputEl.value = cleanName;
              }
            } else {
              alert("Error saving file: " + data.message);
            }
          })
          .catch(err => {
            alert("Error saving questions: " + err);
          });
        })
        .catch(err => {
          console.warn("Save picker error:", err);
          let filename = prompt("Enter a filename or absolute path to save the JSON database:", defaultName);
          if (!filename) return;
          filename = filename.trim();
          if (!filename) return;
          
          const payload = {
            name: document.getElementById("editor-exam-name").value.trim(),
            footer: document.getElementById("editor-exam-footer").value.trim(),
            questions: editorQuestions
          };
          
          fetch("/editor/save", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ filename: filename, data: payload })
          })
          .then(res => res.json())
          .then(data => {
            if (data.status === "ok") {
              showTemporaryEditorAlert("File saved successfully! Location: " + data.filename, "success");
              loadedFilePath = data.filename;
            } else {
              alert("Error saving file: " + data.message);
            }
          });
        });
    }

    function showTemporaryEditorAlert(message, type) {
      let alertBox = document.getElementById("editor-alert-box");
      if (!alertBox) {
        alertBox = document.createElement("div");
        alertBox.id = "editor-alert-box";
        alertBox.className = "alert " + (type === "success" ? "alert-success" : "alert-error");
        alertBox.style.marginTop = "1rem";
        
        const cardEl = document.querySelector("#questions-editor-tab .card");
        cardEl.insertBefore(alertBox, cardEl.firstChild);
      }
      
      alertBox.className = "alert " + (type === "success" ? "alert-success" : "alert-error");
      alertBox.innerHTML = `
        <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
        </svg>
        \${escapeHtml(message)}
      `;
      
      setTimeout(() => {
        if (alertBox) alertBox.remove();
      }, 5000);
    }

    function escapeHtml(text) {
      if (!text) return "";
      return text
        .toString()
        .replace(/&/g, "&amp;")
        .replace(/</g, "&lt;")
        .replace(/>/g, "&gt;")
        .replace(/"/g, "&quot;")
        .replace(/'/g, "&#039;");
    }

    function renderProfileWorkspace() {
      const tbody = document.getElementById('profile-groups-table-body');
      tbody.innerHTML = '';
      
      profileGroups.forEach(gName => {
        const tr = document.createElement('tr');
        tr.style.borderBottom = '1px solid var(--border)';
        tr.innerHTML = `
          <td style="padding: 1rem 1.5rem; font-weight: 600;">${escapeHtml(gName)}</td>
          <td style="padding: 1rem;">
            <input type="number" class="profile-pick" data-group="${escapeHtml(gName)}" value="1" min="1" style="width: 100px; padding: 0.5rem; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 6px; color: var(--text-main); text-align: center;">
          </td>
          <td style="padding: 1rem 1.5rem;">
            <input type="checkbox" class="profile-save-space" data-group="${escapeHtml(gName)}" style="width: auto; cursor: pointer;">
          </td>
        `;
        tbody.appendChild(tr);
      });
    }

    function showTemporaryProfileAlert(message, type) {
      let alertBox = document.getElementById('profile-alert-box');
      if (!alertBox) {
        alertBox = document.createElement('div');
        alertBox.id = 'profile-alert-box';
        alertBox.className = 'alert ' + (type === 'success' ? 'alert-success' : 'alert-error');
        alertBox.style.marginTop = '1rem';
        
        const cardEl = document.querySelector('#exam-profile-editor-tab .card');
        cardEl.insertBefore(alertBox, cardEl.firstChild);
      }
      
      alertBox.className = 'alert ' + (type === 'success' ? 'alert-success' : 'alert-error');
      alertBox.innerHTML = `
        <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
        </svg>
        ${escapeHtml(message)}
      `;
      
      setTimeout(() => {
        if (alertBox) alertBox.remove();
      }, 5000);
    }

    function saveExamProfileToDisk() {
      const examDir = document.getElementById("exam-dir").value.trim();
      if (!examDir) {
        alert("Please select or enter an Exam Destination Directory at the top of the page first!");
        return;
      }

      const totalNum = parseInt(document.getElementById('profile-total-num').value) || 10;
      const seed = document.getElementById('profile-seed').value.trim() || '1a2b3c4d';
      
      const examProfile = { totalNum: totalNum, seed: seed, profile: [] };
      
      profileGroups.forEach(gName => {
        const pickEl = Array.from(document.getElementsByClassName('profile-pick')).find(el => el.getAttribute('data-group') === gName);
        const spaceEl = Array.from(document.getElementsByClassName('profile-save-space')).find(el => el.getAttribute('data-group') === gName);
        
        const pick = parseInt(pickEl ? pickEl.value : '1') || 1;
        const saveSpace = spaceEl ? spaceEl.checked : false;
        
        examProfile.profile.push({ name: gName, num: pick, saveSpace: saveSpace });
      });
      
      let defaultPath = examDir + "/examProfile.json";
      fetch("/pick-save-file?default=" + encodeURIComponent(defaultPath) + "&title=" + encodeURIComponent("Save Exam Profile JSON"))
        .then(res => res.json())
        .then(pickerData => {
          if (pickerData.status !== "ok" || !pickerData.path) return;
          const filename = pickerData.path;
          
          fetch("/editor/save", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ filename: filename, data: examProfile })
          })
          .then(res => res.json())
          .then(data => {
            if (data.status === "ok") {
              showTemporaryProfileAlert("Exam profile saved successfully to " + data.filename, "success");
            } else {
              alert("Error saving exam profile: " + data.message);
            }
          })
          .catch(err => alert("Error saving exam profile: " + err));
        });
    }

    let markProfileGroups = [];

    function browseAndLoadMarkExamProfile() {
      browseFile('mark-exam-profile-path', '*.json', 'Select Exam Profile JSON');
      const checkInterval = setInterval(() => {
        const path = document.getElementById('mark-exam-profile-path').value;
        if (path) {
          clearInterval(checkInterval);
          loadMarkExamProfileFromPath();
        }
      }, 500);
      setTimeout(() => clearInterval(checkInterval), 60000);
    }

    function loadMarkExamProfileFromPath() {
      const path = document.getElementById('mark-exam-profile-path').value.trim();
      if (!path) {
        alert('Please enter or browse for an exam profile JSON file path.');
        return;
      }
      
      fetch('/editor/load?file=' + encodeURIComponent(path))
        .then(res => res.json())
        .then(json => {
          if (json.status === 'error') {
            alert('Error loading file: ' + json.message);
            return;
          }
          
          const groupsList = json.profile || json.groups;
          if (!groupsList || !Array.isArray(groupsList)) {
             alert("Invalid exam profile JSON: missing 'profile' list.");
             return;
          }
          
          markProfileGroups = groupsList.map(g => g.name.trim()).filter(Boolean);
          renderMarkProfileWorkspace();
          document.getElementById('mark-profile-workspace').style.display = 'block';
          showTemporaryMarkProfileAlert('Successfully loaded ' + markProfileGroups.length + ' groups from ' + path + '!', 'success');
        })
        .catch(err => alert('Error loading exam profile: ' + err));
    }

    function renderMarkProfileWorkspace() {
      const tbody = document.getElementById('mark-profile-groups-table-body');
      tbody.innerHTML = '';
      
      markProfileGroups.forEach(gName => {
        const tr = document.createElement('tr');
        tr.style.borderBottom = '1px solid var(--border)';
        tr.innerHTML = `
          <td style="padding: 1rem 1.5rem; font-weight: 600;">${escapeHtml(gName)}</td>
          <td style="padding: 1rem;">
            <input type="number" class="mark-pos-mark" data-group="${escapeHtml(gName)}" value="1" min="0" step="0.25" style="width: 100px; padding: 0.5rem; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 6px; color: var(--text-main); text-align: center;">
          </td>
          <td style="padding: 1rem;">
            <input type="number" class="mark-neg-mark" data-group="${escapeHtml(gName)}" value="-0.25" step="0.25" style="width: 100px; padding: 0.5rem; background-color: var(--bg-tertiary); border: 1px solid var(--border); border-radius: 6px; color: var(--text-main); text-align: center;">
          </td>
        `;
        tbody.appendChild(tr);
      });
    }

    function showTemporaryMarkProfileAlert(message, type) {
      let alertBox = document.getElementById('mark-profile-alert-box');
      if (!alertBox) {
        alertBox = document.createElement('div');
        alertBox.id = 'mark-profile-alert-box';
        alertBox.className = 'alert ' + (type === 'success' ? 'alert-success' : 'alert-error');
        alertBox.style.marginTop = '1rem';
        
        const cardEl = document.querySelector('#mark-profile-editor-tab .card');
        cardEl.insertBefore(alertBox, cardEl.firstChild);
      }
      
      alertBox.className = 'alert ' + (type === 'success' ? 'alert-success' : 'alert-error');
      alertBox.innerHTML = `
        <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24">
          <path stroke-linecap="round" stroke-linejoin="round" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/>
        </svg>
        ${escapeHtml(message)}
      `;
      
      setTimeout(() => {
        if (alertBox) alertBox.remove();
      }, 5000);
    }

    function saveMarkProfileToDisk() {
      const examDir = document.getElementById("exam-dir").value.trim();
      if (!examDir) {
        alert("Please select or enter an Exam Destination Directory at the top of the page first!");
        return;
      }

      if (markProfileGroups.length === 0) {
        alert("Please load an exam profile first.");
        return;
      }
      
      const markProfile = [];
      markProfileGroups.forEach(gName => {
        const posEl = Array.from(document.getElementsByClassName('mark-pos-mark')).find(el => el.getAttribute('data-group') === gName);
        const negEl = Array.from(document.getElementsByClassName('mark-neg-mark')).find(el => el.getAttribute('data-group') === gName);
        
        const posmark = parseFloat(posEl ? posEl.value : '1') || 1;
        const negmark = parseFloat(negEl ? negEl.value : '0') || 0;
        
        markProfile.push({ name: gName, posmark: posmark, negmark: negmark });
      });
      
      let defaultPath = examDir + "/markProfile.json";
      fetch("/pick-save-file?default=" + encodeURIComponent(defaultPath) + "&title=" + encodeURIComponent("Save Mark Profile JSON"))
        .then(res => res.json())
        .then(pickerData => {
          if (pickerData.status !== "ok" || !pickerData.path) return;
          const filename = pickerData.path;
          
          fetch("/editor/save", {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({ filename: filename, data: markProfile })
          })
          .then(res => res.json())
          .then(data => {
            if (data.status === "ok") {
              showTemporaryMarkProfileAlert("Mark profile saved successfully to " + data.filename, "success");
            } else {
              alert("Error saving mark profile: " + data.message);
            }
          })
          .catch(err => alert("Error saving mark profile: " + err));
        });
    }

    function browseFile(inputId, filter, title) {
      const url = '/pick-file?filter=' + encodeURIComponent(filter) + '&title=' + encodeURIComponent(title);
      fetch(url)
        .then(res => res.json())
        .then(data => {
          if (data.status === 'ok' && data.path) {
            document.getElementById(inputId).value = data.path;
          }
        })
        .catch(err => console.warn('File picker error:', err));
    }

    document.addEventListener("DOMContentLoaded", () => {
      const initialTab = "%s";
      const btn = document.querySelector(`.tab-btn[onclick*="${initialTab}"]`);
      if (btn) {
        switchTab(initialTab, btn);
      } else {
        const defaultBtn = document.querySelector('.tab-btn[onclick*="questions-editor-tab"]');
        if (defaultBtn) {
          switchTab('questions-editor-tab', defaultBtn);
        }
      }
    });
  </script>
</body>
</html>
|} active_tab dest_dir output_style status_alert dest_dir files_badges console_log active_tab

(* Handle GET / request *)
let handle_index _env _req =
  let html = render_hub ~status_alert:"" ~console_log:"" ~generated_files:[] ~dest_dir:"exam_backup" ~active_tab:"questions-editor-tab" in
  Yume.Server.respond_html html

(* Handle POST /run execution *)
let handle_run _env req =
  (* 1. Extract dest_dir *)
  let dest_dir =
    match Yume.Server.formdata "dest_dir" req with
    | Ok fd when String.length (String.strip fd.content) > 0 -> String.strip fd.content
    | _ -> "exam_backup"
  in

  (* 2. Extract active_tab *)
  let active_tab =
    match Yume.Server.formdata "active_tab" req with
    | Ok fd when String.length (String.strip fd.content) > 0 -> String.strip fd.content
    | _ -> "create-exam-tab"
  in

  (* 3. Extract action *)
  let action =
    match Yume.Server.formdata "action" req with
    | Ok fd -> fd.content
    | _ -> "create"
  in

  (* 4. Resolve questions file *)
  let questions_file =
    match Yume.Server.formdata "questions_path" req with
    | Ok fd when String.length (String.strip fd.content) > 0 -> String.strip fd.content
    | _ -> "example/questions.json"
  in

  (* 5. Prepare inputs in the Destination Directory *)
  let _ =
    mkdir_p dest_dir;
    if String.equal action "create" then (
      (* Copy custom examProfile.json to dest_dir/examProfile.json if provided *)
      let target_profile = Stdlib.Filename.concat dest_dir "examProfile.json" in
      (match Yume.Server.formdata "exam_profile_path" req with
      | Ok fd when String.length (String.strip fd.content) > 0 ->
          let _ = copy_file (String.strip fd.content) target_profile in ()
      | _ ->
          if not (Stdlib.Sys.file_exists target_profile) then
            let _ = copy_file "example/examProfile.json" target_profile in ());
      
      (* Copy _latexPreample_ to dest_dir if not exists *)
      let target_preample = Stdlib.Filename.concat dest_dir "_latexPreample_" in
      if not (Stdlib.Sys.file_exists target_preample) then
        let _ = copy_file "example/_latexPreample_" target_preample in ()
        
    ) else if String.equal action "mark" then (
      (* Copy custom givenAnswers.csv to dest_dir/givenAnswers.csv if provided *)
      let target_answers = Stdlib.Filename.concat dest_dir "givenAnswers.csv" in
      let _ =
        match Yume.Server.formdata "given_answers_path" req with
        | Ok fd when String.length (String.strip fd.content) > 0 ->
            let _ = copy_file (String.strip fd.content) target_answers in ()
        | _ ->
            if not (Stdlib.Sys.file_exists target_answers) then
              let _ = copy_file "example/givenAnswers.csv" target_answers in ()
      in
      (* Copy custom markProfile.json to dest_dir/markProfile.json if provided *)
      let target_mark_profile = Stdlib.Filename.concat dest_dir "markProfile.json" in
      let _ =
        match Yume.Server.formdata "mark_profile_path" req with
        | Ok fd when String.length (String.strip fd.content) > 0 ->
            let _ = copy_file (String.strip fd.content) target_mark_profile in ()
        | _ ->
            if not (Stdlib.Sys.file_exists target_mark_profile) then
              let _ = copy_file "example/markProfile.json" target_mark_profile in ()
      in ()
    )
  in

  (* 6. Execute the binary as a sub-process *)
  let binary_path = "_build/default/bin/mtest.exe" in
  if not (Stdlib.Sys.file_exists binary_path) then
    let status_alert = {|
      <div class="alert alert-error">
        <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/></svg>
        Error: CLI binary is not compiled yet. Please build the project.
      </div>
    |} in
    let html = render_hub ~status_alert ~console_log:"Error: _build/default/bin/mtest.exe not found." ~generated_files:[] ~dest_dir ~active_tab in
    Yume.Server.respond_html html
  else
    let cmd =
      if String.equal action "create" then
        Printf.sprintf "%s create -dest %s %s" binary_path dest_dir questions_file
      else if String.equal action "mark" then
        Printf.sprintf "%s mark -dest %s %s" binary_path dest_dir (Stdlib.Filename.concat dest_dir "givenAnswers.csv")
      else
        Printf.sprintf "%s %s %s" binary_path action questions_file
    in
    let exit_code, console_log = run_command cmd in

    (* If backup is successful, copy directory *)
    let _ =
      if exit_code = 0 && String.equal action "backup" then
        copy_directory "exam_backup" dest_dir
    in

    let status_alert =
      if exit_code = 0 then
        Printf.sprintf {|
          <div class="alert alert-success">
            <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"/></svg>
            Action '%s' completed successfully! Output copied to '%s'.
          </div>
        |} action dest_dir
      else
        Printf.sprintf {|
          <div class="alert alert-error">
            <svg width="20" height="20" fill="none" stroke="currentColor" stroke-width="2" viewBox="0 0 24 24"><path stroke-linecap="round" stroke-linejoin="round" d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"/></svg>
            Action '%s' failed with exit code %d. Review logs below.
          </div>
        |} action exit_code
    in

    (* Gather files in the target directory *)
    let generated_files =
      if Stdlib.Sys.file_exists dest_dir && Stdlib.Sys.is_directory dest_dir then
        Stdlib.Sys.readdir dest_dir |> Array.to_list |> List.sort ~compare:String.compare
      else []
    in

    let html = render_hub ~status_alert ~console_log ~generated_files ~dest_dir ~active_tab in
    Yume.Server.respond_html html

(* Handle GET /dirs query *)
let handle_dirs _env req =
  let current_path =
    match Yume.Server.query_opt "path" req with
    | Some p when String.length p > 0 -> p
    | _ -> "."
  in
  let absolute_path = current_path in
  let dirs =
    try
      let all = Stdlib.Sys.readdir absolute_path |> Array.to_list in
      let subdirs =
        all
        |> List.filter ~f:(fun f ->
             let full = Stdlib.Filename.concat absolute_path f in
             try Stdlib.Sys.is_directory full with _ -> false
           )
        |> List.sort ~compare:String.compare
      in
      if String.equal current_path "." || String.equal current_path "" then
        subdirs
      else
        ".." :: subdirs
    with _ -> []
  in
  let json = `List (List.map dirs ~f:(fun d -> `String d)) in
  Yume.Server.respond_yojson json

(* Handle GET /editor/load query *)
let handle_editor_load _env req =
  match Yume.Server.query_opt "file" req with
  | Some f when String.length f > 0 -> (
      try
        let content = In_channel.read_all f in
        let json = Yojson.Safe.from_string content in
        Yume.Server.respond_yojson json
      with ex ->
        let json = `Assoc [("status", `String "error"); ("message", `String (Stdlib.Printexc.to_string ex))] in
        Yume.Server.respond_yojson json
    )
  | _ ->
      let json = `Assoc [("status", `String "error"); ("message", `String "Missing file parameter")] in
      Yume.Server.respond_yojson json

(* Handle POST /editor/save execution *)
let handle_editor_save _env req =
  try
    let body_str = Yume.Server.body req in
    let json = Yojson.Safe.from_string body_str in
    let (filename, data) =
      match json with
      | `Assoc l ->
          let f =
            match List.Assoc.find l ~equal:String.equal "filename" with
            | Some (`String s) -> s
            | _ -> failwith "Missing or invalid filename"
          in
          let d =
            match List.Assoc.find l ~equal:String.equal "data" with
            | Some d -> d
            | _ -> failwith "Missing data"
          in
          (f, d)
      | _ -> failwith "Request body must be a JSON object"
    in
    
    let full_path =
      let with_ext =
        if String.is_suffix filename ~suffix:".json" then filename
        else filename ^ ".json"
      in
      if String.is_prefix with_ext ~prefix:"/" || Stdlib.String.contains with_ext '/' then
        with_ext
      else
        Stdlib.Filename.concat "example" with_ext
    in
    
    let _ =
      let dir = Stdlib.Filename.dirname full_path in
      if not (String.equal dir "." || String.equal dir "/") then (
        mkdir_p dir
      )
    in
    
    let content = Yojson.Safe.pretty_to_string data in
    Out_channel.write_all full_path ~data:content;
    
    let resp = `Assoc [
      ("status", `String "ok");
      ("filename", `String full_path);
      ("message", `String ("Saved successfully to " ^ full_path))
    ] in
    Yume.Server.respond_yojson resp
  with ex ->
    let resp = `Assoc [
      ("status", `String "error");
      ("message", `String (Stdlib.Printexc.to_string ex))
    ] in
    Yume.Server.respond_yojson resp



(* Handle GET /pick-dir query *)
let handle_pick_dir _env _req =
  let cmd = "zenity --file-selection --directory --title=\"Select Destination Directory\"" in
  let exit_code, output = run_command cmd in
  let path = get_last_line output in
  let json =
    if exit_code = 0 && String.length path > 0 then
      `Assoc [("status", `String "ok"); ("path", `String path)]
    else
      `Assoc [("status", `String "error"); ("message", `String "Cancelled or failed")]
  in
  Yume.Server.respond_yojson json

(* Handle GET /pick-file query - supports filter and title params, and directory mode *)
let handle_pick_file _env req =
  let filter =
    match Yume.Server.query_opt "filter" req with
    | Some f when String.length f > 0 -> f
    | _ -> "*.json"
  in
  let title =
    match Yume.Server.query_opt "title" req with
    | Some t when String.length t > 0 -> t
    | _ -> "Select File"
  in
  let cmd =
    if String.equal filter "directory" then
      Printf.sprintf "zenity --file-selection --directory --title=\"%s\"" title
    else
      Printf.sprintf "zenity --file-selection --file-filter=\"%s\" --title=\"%s\"" filter title
  in
  let exit_code, output = run_command cmd in
  let path = get_last_line output in
  let json =
    if exit_code = 0 && String.length path > 0 then
      `Assoc [("status", `String "ok"); ("path", `String path)]
    else
      `Assoc [("status", `String "error"); ("message", `String "Cancelled or failed")]
  in
  Yume.Server.respond_yojson json

(* Handle GET /pick-save-file query *)
let handle_pick_save_file _env req =
  let default_filename =
    match Yume.Server.query_opt "default" req with
    | Some d when String.length d > 0 -> d
    | _ -> "my_questions.json"
  in
  (* Create parent directory recursively if it does not exist yet to prevent Zenity dialog failure *)
  let _ =
    let dir = Stdlib.Filename.dirname default_filename in
    if not (String.equal dir "." || String.equal dir "/") then (
      mkdir_p dir
    )
  in
  let title =
    match Yume.Server.query_opt "title" req with
    | Some t when String.length t > 0 -> t
    | _ -> "Save JSON File"
  in
  let cmd =
    Printf.sprintf
      "zenity --file-selection --save --confirm-overwrite --file-filter=\"*.json\" --title=\"%s\" --filename=\"%s\""
      title default_filename
  in
  let exit_code, output = run_command cmd in
  let path = get_last_line output in
  let json =
    if exit_code = 0 && String.length path > 0 then
      `Assoc [("status", `String "ok"); ("path", `String path)]
    else
      `Assoc [("status", `String "error"); ("message", `String "Cancelled or failed")]
  in
  Yume.Server.respond_yojson json

(* Main Server Entrypoint *)
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let listen = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let router =
    let open Yume.Server.Router in
    use [
      get "/" handle_index;
      get "/dirs" handle_dirs;
      get "/pick-dir" handle_pick_dir;
      get "/pick-file" handle_pick_file;
      get "/pick-save-file" handle_pick_save_file;
      get "/editor/load" handle_editor_load;
      post "/editor/save" handle_editor_save;
      post "/run" handle_run;
    ] Yume.Server.default_handler
  in
  Yume.Server.start_server env ~sw ~listen router @@ fun _socket ->
  Printf.printf "\n=== MultiTest Server Running ===\nAccess the control hub at: http://localhost:8080\n=================================\n%!"
