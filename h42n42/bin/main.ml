open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js.To_dom
open Lwt.Syntax
open Js_of_ocaml_lwt

let global_creets = ref []

let game_width = 1600.0
let game_height = 800.0

let global_base_speed = ref 50.0
let speed_increase_per_frame = 5.0

let update_global_speed () =
  if !global_base_speed < 150.0 then
    global_base_speed := !global_base_speed +. speed_increase_per_frame

let game_container = ref None

let contamination_thread = ref None

let dragged_creet = ref None

let setup_drag_system () =
  Dom_html.document##.onmousemove := Dom_html.handler (fun ev ->
    (match !dragged_creet with
     | Some creet -> 
         creet#update_drag (Js.to_float ev##.clientX) (Js.to_float ev##.clientY)
     | None -> ());
    Js._true
  );
  
  Dom_html.document##.onmouseup := Dom_html.handler (fun _ ->
    (match !dragged_creet with
     | Some creet -> 
         creet#stop_drag ();
         dragged_creet := None
     | None -> ());
    Js._true
  )

let update_global_speed_for_all_creets () =
  update_global_speed ();
  List.iter (fun creet ->
    creet#set_global_speed !global_base_speed
  ) !global_creets
  
  let create_creet () =
    match !game_container with
    | None -> None
    | Some container ->
        let new_creet = new Creet.creet !global_base_speed in
        new_creet#initialize container;
        global_creets := new_creet :: !global_creets;
  
        (match new_creet#get_dom_element with
         | Some elem ->
             elem##.onmousedown := Dom_html.handler (fun ev ->
               dragged_creet := Some new_creet;
               new_creet#start_drag (Js.to_float ev##.clientX) (Js.to_float ev##.clientY);
               ev##preventDefault;
               Js._false
             )
         | None -> ());
        Some new_creet

let start_global_speed_system () =
  let rec speed_loop () =
    update_global_speed_for_all_creets ();
    if Random.int 10 = 0 then  (* 1 chance sur 20 *)
      ignore (create_creet ());
    let* () = Lwt_js.sleep 2.0 in
    speed_loop ()
  in
  Lwt.async (fun () -> speed_loop ())

let () = Random.self_init ()


let create_creets count =
  for _ = 1 to count do
    ignore (create_creet ())
  done

let cleanup_dead_creets () =
  let (alive, dead) = List.partition (fun c -> c#is_alive) !global_creets in
  
  List.iter (fun creet -> 
    creet#stop_behavior_thread ();
    creet#remove_from_dom ()
  ) dead;
  
  global_creets := alive

let start_contamination_system () =
  let rec contamination_loop () =
    let creets = !global_creets in
    List.iter (fun creet1 ->
      if creet1#get_status != "healthy" && creet1#get_status != "dead" && creet1#is_alive then
        List.iter (fun creet2 ->
          if creet1 != creet2 && creet2#is_alive then
            ignore (creet1#try_contaminate creet2)
        ) creets
    ) creets;
    
    cleanup_dead_creets ();
    
    let* () = Lwt_js.sleep 0.1 in
    contamination_loop ()
  in
  
  let thread = contamination_loop () in
  contamination_thread := Some thread;
  Lwt.async (fun () -> thread)

let get_stats () =
  let creets = !global_creets in
  let total = List.length creets in
  let healthy = List.length (List.filter (fun c -> c#get_status = "healthy") creets) in
  let sick = List.length (List.filter (fun c -> c#get_status = "sick") creets) in
  let berserk = List.length (List.filter (fun c -> c#get_status = "berserk") creets) in
  let mean = List.length (List.filter (fun c -> c#get_status = "mean") creets) in
  let dead = List.length (List.filter (fun c -> c#get_status = "dead" || c#get_status = "dead_deflating") creets) in
  (total, healthy, sick, berserk, mean, dead)

let update_stats_display () =
  let (total, healthy, sick, berserk, mean, dead) = get_stats () in
  match Dom_html.getElementById_opt "stats" with
  | Some elem ->
      let stats_text = Printf.sprintf 
        "Total: %d | Healthy: %d | Sick: %d | Berserk: %d | Mean: %d | Dead: %d | Speed: %.1f" 
        total healthy sick berserk mean dead !global_base_speed in
      elem##.textContent := Js.some (Js.string stats_text)
  | None -> ()

let start_stats_updater () =
  let rec stats_loop () =
    update_stats_display ();
    let* () = Lwt_js.sleep 1.0 in
    stats_loop ()
  in
  Lwt.async (fun () -> stats_loop ())

let add_sick_creet () =
  match create_creet () with
  | Some creet -> creet#set_status "sick"
  | None -> ()

let infect_random_creet () =
  let healthy_creets = List.filter (fun c -> 
    c#get_status = "healthy" && c#is_alive
  ) !global_creets in
  
  match healthy_creets with
  | [] -> ()
  | creets ->
      let random_creet = List.nth creets (Random.int (List.length creets)) in
      random_creet#set_status "sick"

let clear_all_creets () =
  List.iter (fun creet ->
    creet#stop_behavior_thread ();
    creet#remove_from_dom ()
  ) !global_creets;
  global_creets := []

(* Créer la page *)
let create_page () =
  Dom_html.document##.title := Js.string "H42N42";

  let left_zone = div 
    ~a:[
      a_style "position: absolute; left: 0; top: 0; width: 80px; 
               height: 100%; background-color: green; z-index: 1;
               border-right: 2px solid #333;"
    ] [] in
  
  let right_zone = div 
    ~a:[
      a_style "position: absolute; right: 0; top: 0; width: 80px; 
               height: 100%; background-color: purple; z-index: 1;
               border-left: 2px solid #333;"
    ] [] in
  
  let game_area = div 
    ~a:[
      a_id "game-area";
      a_style (Printf.sprintf 
        "position: relative; width: %fpx; height: %fpx; 
         border: 3px solid #333; margin: 20px auto; 
         background: linear-gradient(45deg, #e8f5e8, #f0f8f0); 
         overflow: hidden; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"
        game_width game_height)
    ] 
    [left_zone; right_zone] in
  
  let control_panel = div 
    ~a:[a_style "text-align: center; margin: 20px; padding: 15px; 
                 background: #f5f5f5; border-radius: 8px;"]
    [
      h3 [txt "Simulation Controls"];
      
      button 
        ~a:[
          a_onclick (fun _ -> ignore (create_creet ()); false);
          a_style "margin: 5px; padding: 8px 15px; background: #4CAF50; 
                   color: white; border: none; border-radius: 4px; cursor: pointer;"
        ]
        [txt "➕ Add Healthy Creet"];
      
      txt " ";
      
      button 
        ~a:[
          a_onclick (fun _ -> add_sick_creet (); false);
          a_style "margin: 5px; padding: 8px 15px; background: #F44336; 
                   color: white; border: none; border-radius: 4px; cursor: pointer;"
        ]
        [txt "🦠 Add Sick Creet"];
      
      txt " ";
      
      button 
        ~a:[
          a_onclick (fun _ -> infect_random_creet (); false);
          a_style "margin: 5px; padding: 8px 15px; background: #FF9800; 
                   color: white; border: none; border-radius: 4px; cursor: pointer;"
        ]
        [txt "💉 Random Infection"];
      
      txt " ";
      
      button 
        ~a:[
          a_onclick (fun _ -> clear_all_creets (); false);
          a_style "margin: 5px; padding: 8px 15px; background: #9E9E9E; 
                   color: white; border: none; border-radius: 4px; cursor: pointer;"
        ]
        [txt "🧹 Clear All"];
    ] in
  
  let stats_display = div 
    ~a:[
      a_id "stats";
      a_style "text-align: center; font-size: 18px; font-weight: bold; 
               margin: 15px; padding: 10px; background: #e3f2fd; 
               border-radius: 6px; border-left: 4px solid #2196F3;"
    ]
    [txt "Total: 0 | Healthy: 0 | Sick: 0 | Dead: 0"] in
  
  let instructions = div 
    ~a:[a_style "text-align: center; margin: 15px; color: #666; font-size: 14px;"]
    [
      h2 [txt "🎯 Goal: Protect the Creets at all costs!"];
    ] in

  let legend = div 
    ~a:[a_style "text-align: center; margin: 15px; padding: 10px; 
                 background: #fafafa; border-radius: 6px;"]
    [
      h4 [txt "Legend"];
      span ~a:[a_style "margin: 0 10px;"] [txt "😊 Healthy"];
      span ~a:[a_style "margin: 0 10px;"] [txt "🤢 Sick"];
      span ~a:[a_style "margin: 0 10px;"] [txt "💀 Dead"];
      span ~a:[a_style "margin: 0 10px;"] [txt "👹 Berserk"];
      span ~a:[a_style "margin: 0 10px;"] [txt "😈 Mean"];
    ] in
  
  let page_content = [
    h1 ~a:[a_style "text-align: center; color: #2c3e50;"] 
       [txt "🦠 H42N42 - Fight the Infection!"];
    instructions;
    stats_display;
    control_panel;
    legend;
    game_area;
  ] in
  
  let body = Dom_html.document##.body in
  body##.innerHTML := Js.string "";
  
  List.iter (fun elem -> 
    Dom.appendChild body (of_element elem)
  ) page_content;
  
  game_container := Some (of_element game_area);
  
  setup_drag_system ();
  start_contamination_system ();
  start_stats_updater ();
  start_global_speed_system ();
  
  create_creets 8

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    create_page ();
    Js._false
  )