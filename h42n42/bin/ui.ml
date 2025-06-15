open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js.To_dom

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
        Game_config.game_width Game_config.game_height)
    ] 
    [left_zone; right_zone] in
  
  let control_panel = div 
    ~a:[a_style "text-align: center; margin: 20px; padding: 15px; 
                 background: #f5f5f5; border-radius: 8px;"]
    [
      h3 [txt "Simulation Controls"];
      
      button 
        ~a:[
          a_onclick (fun _ -> ignore (Game_state.create_creet ()); false);
          a_style "margin: 5px; padding: 8px 15px; background: #4CAF50; 
                   color: white; border: none; border-radius: 4px; cursor: pointer;"
        ]
        [txt "➕ Add Healthy Creet"];
      
      txt " ";
      
      button 
        ~a:[
          a_onclick (fun _ -> Game_state.add_sick_creet (); false);
          a_style "margin: 5px; padding: 8px 15px; background: #F44336; 
                   color: white; border: none; border-radius: 4px; cursor: pointer;"
        ]
        [txt "🦠 Add Sick Creet"];
      
      txt " ";
      
      button 
        ~a:[
          a_onclick (fun _ -> Game_state.infect_random_creet (); false);
          a_style "margin: 5px; padding: 8px 15px; background: #FF9800; 
                   color: white; border: none; border-radius: 4px; cursor: pointer;"
        ]
        [txt "💉 Random Infection"];
      
      txt " ";
      
      button 
        ~a:[
          a_onclick (fun _ -> Game_state.clear_all_creets (); false);
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
  
  Game_state.state.game_container <- Some (of_element game_area);
  
  Game_state.start_game_timer ();
  
  Game_systems.start_all_systems ();
  
  Game_state.create_creets 8