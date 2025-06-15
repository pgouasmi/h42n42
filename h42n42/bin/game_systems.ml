open Js_of_ocaml
open Lwt.Syntax
open Js_of_ocaml_lwt

let start_contamination_system () =
  let rec contamination_loop () =
    let creets = Game_state.state.creets in
    
    List.iter (fun creet ->
      if creet#get_status = "mean" && creet#is_alive then
        creet#update_chase_behavior creets
    ) creets;
    
    List.iter (fun creet1 ->
      if creet1#get_status != "healthy" && creet1#get_status != "dead" && creet1#get_status != "dead_deflating" && creet1#is_alive then
        List.iter (fun creet2 ->
          if creet1 != creet2 && creet2#is_alive then
            ignore (creet1#try_contaminate creet2)
        ) creets
    ) creets;
    
    Game_state.cleanup_dead_creets ();
    
    (* Vérifier la fin de partie *)
    if Game_state.check_game_over () then
      Game_events.show_game_over_popup ();
    
    let* () = Lwt_js.sleep Game_config.contamination_frequency in
    contamination_loop ()
  in
  Lwt.async (fun () -> contamination_loop ())

let start_global_speed_system () =
  let rec speed_loop () =
    Game_state.update_global_speed_for_all_creets ();
    
    if Random.int Game_config.new_creet_spawn_chance = 0 && List.length Game_state.state.creets < Game_config.max_creets then
      ignore (Game_state.create_creet ());
    
    let* () = Lwt_js.sleep Game_config.speed_update_frequency in
    speed_loop ()
  in
  Lwt.async (fun () -> speed_loop ())

let start_stats_updater () =
  let rec stats_loop () =
    Game_state.update_stats_display ();
    let* () = Lwt_js.sleep Game_config.stats_update_frequency in
    stats_loop ()
  in
  Lwt.async (fun () -> stats_loop ())

let setup_drag_system () =
  Dom_html.document##.onmousemove := Dom_html.handler (fun ev ->
    (match Game_state.state.dragged_creet with
     | Some creet -> 
         creet#update_drag (Js.to_float ev##.clientX) (Js.to_float ev##.clientY)
     | None -> ());
    Js._true
  );
  
  Dom_html.document##.onmouseup := Dom_html.handler (fun _ ->
    (match Game_state.state.dragged_creet with
     | Some creet -> 
         creet#stop_drag ();
         Game_state.state.dragged_creet <- None
     | None -> ());
    Js._true
  )

let start_all_systems () =
  setup_drag_system ();
  start_contamination_system ();
  start_stats_updater ();
  start_global_speed_system ()