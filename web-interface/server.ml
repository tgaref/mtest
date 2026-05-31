(* server.ml - MultiTest Yume Web Interface Entrypoint *)

open Core

(* Main Server Entrypoint *)
let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let listen = `Tcp (Eio.Net.Ipaddr.V4.loopback, 8080) in
  let router =
    let open Yume.Server.Router in
    use [
      get "/" Controller.handle_index;
      get "/style.css" Controller.handle_style;
      get "/dirs" Controller.handle_dirs;
      get "/pick-dir" Controller.handle_pick_dir;
      get "/pick-file" Controller.handle_pick_file;
      get "/pick-save-file" Controller.handle_pick_save_file;
      get "/editor/load" Controller.handle_editor_load;
      post "/editor/save" Controller.handle_editor_save;
      post "/run" Controller.handle_run;
    ] Yume.Server.default_handler
  in
  Yume.Server.start_server env ~sw ~listen router @@ fun _socket ->
  Printf.printf "\n=== MultiTest Server Running ===\nAccess the control hub at: http://localhost:8080\n=================================\n%!"
