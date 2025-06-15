open Js_of_ocaml

type game_state = {
  mutable running: bool;
  mutable canvas_width: float;
  mutable canvas_height: float;
  mutable global_base_speed: float;
  mutable game_over_shown: bool;
  mutable creets: Creet.creet list;
  mutable game_container: Dom_html.element Js.t option;
  mutable dragged_creet: Creet.creet option;
  mutable start_time: float;
  mutable end_time: float;
}

let state = { 
  running = true; 
  canvas_width = Game_config.game_width; 
  canvas_height = Game_config.game_height;
  global_base_speed = Game_config.initial_speed;
  game_over_shown = false;
  creets = [];
  game_container = None;
  dragged_creet = None;
  start_time = 0.0;
  end_time = 0.0;
}

let stop_game () = state.running <- false
let is_running () = state.running

let update_global_speed () =
  state.global_base_speed <- state.global_base_speed +. Game_config.speed_increase_per_frame

let create_creet () =
  match state.game_container with
  | None -> None
  | Some container ->
      let new_creet = new Creet.creet state.global_base_speed in
      new_creet#initialize container;
      state.creets <- new_creet :: state.creets;
      
      (match new_creet#get_dom_element with
       | Some elem ->
           elem##.onmousedown := Dom_html.handler (fun ev ->
             state.dragged_creet <- Some new_creet;
             new_creet#start_drag (Js.to_float ev##.clientX) (Js.to_float ev##.clientY);
             ev##preventDefault;
             Js._false
           )
       | None -> ());
      
      Some new_creet

let create_creets count =
  for _ = 1 to count do
    ignore (create_creet ())
  done

let cleanup_dead_creets () =
  let (alive, dead) = List.partition (fun c -> c#is_alive) state.creets in
  
  List.iter (fun creet -> 
    creet#stop_behavior_thread ();
    creet#remove_from_dom ()
  ) dead;
  
  state.creets <- alive

let clear_all_creets () =
  List.iter (fun creet ->
    creet#stop_behavior_thread ();
    creet#remove_from_dom ()
  ) state.creets;
  state.creets <- []

let add_sick_creet () =
  match create_creet () with
  | Some creet -> creet#set_status "sick"
  | None -> ()

let infect_random_creet () =
  let healthy_creets = List.filter (fun c -> 
    c#get_status = "healthy" && c#is_alive
  ) state.creets in
  
  match healthy_creets with
  | [] -> ()
  | creets ->
      let random_creet = List.nth creets (Random.int (List.length creets)) in
      random_creet#set_status "sick"

let start_game_timer () =
  state.start_time <- Js.to_float (new%js Js.date_now)##getTime

let stop_game_timer () =
  state.end_time <- Js.to_float (new%js Js.date_now)##getTime

let get_game_duration () =
  if state.end_time > 0.0 then
    (state.end_time -. state.start_time) /. 1000.0
  else
    (Js.to_float (new%js Js.date_now)##getTime -. state.start_time) /. 1000.0

let format_time seconds =
  let mins = int_of_float (seconds /. 60.0) in
  let secs = int_of_float (mod_float seconds 60.0) in
  Printf.sprintf "%02d:%02d" mins secs
let get_stats () =
  let total = List.length state.creets in
  let healthy = List.length (List.filter (fun c -> c#get_status = "healthy") state.creets) in
  let sick = List.length (List.filter (fun c -> c#get_status = "sick") state.creets) in
  let berserk = List.length (List.filter (fun c -> c#get_status = "berserk") state.creets) in
  let mean = List.length (List.filter (fun c -> c#get_status = "mean") state.creets) in
  let dead = List.length (List.filter (fun c -> c#get_status = "dead" || c#get_status = "dead_deflating") state.creets) in
  (total, healthy, sick, berserk, mean, dead)

let update_stats_display () =
  let (total, healthy, sick, berserk, mean, dead) = get_stats () in
  let duration = get_game_duration () in
  let formatted_time = format_time duration in
  match Dom_html.getElementById_opt "stats" with
  | Some elem ->
      let stats_text = Printf.sprintf 
        "Time: %s | Total: %d | Healthy: %d | Sick: %d | Berserk: %d | Mean: %d | Dead: %d | Speed: %.1f" 
        formatted_time total healthy sick berserk mean dead state.global_base_speed in
      elem##.textContent := Js.some (Js.string stats_text)
  | None -> ()

let check_game_over () =
  if not state.game_over_shown then (
    let healthy_count = List.length (List.filter (fun c -> 
      c#get_status = "healthy" && c#is_alive
    ) state.creets) in
    
    if healthy_count = 0 && List.length state.creets > 0 then (
      state.game_over_shown <- true;
      stop_game_timer ();
      true
    ) else false
  ) else false

let reset_game () =
  clear_all_creets ();
  state.running <- true;
  state.global_base_speed <- Game_config.initial_speed;
  state.game_over_shown <- false;
  state.dragged_creet <- None;
  state.start_time <- 0.0;
  state.end_time <- 0.0

let update_global_speed_for_all_creets () =
  update_global_speed ();
  List.iter (fun creet ->
    creet#set_global_speed state.global_base_speed
  ) state.creets