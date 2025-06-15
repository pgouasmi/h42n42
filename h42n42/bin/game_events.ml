open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js.To_dom

let create_page_callback = ref (fun () -> ())

let set_create_page_callback f = create_page_callback := f


let start_game () =
  let overlays = Dom_html.document##querySelectorAll (Js.string "div[style*='position: fixed']") in
  for i = 0 to overlays##.length - 1 do
    match Js.Opt.to_option (overlays##item i) with
    | Some elem -> 
        (match Js.Opt.to_option elem##.parentNode with
         | Some parent -> 
             let elem_as_node = (elem :> Dom.node Js.t) in
             ignore (parent##removeChild elem_as_node)
         | None -> ())
    | None -> ()
  done;
  
  !create_page_callback ()

let show_intro_popup () =
  let overlay = div 
    ~a:[
      a_style "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
               background: rgba(0,0,0,0.9); z-index: 2000; display: flex; 
               align-items: center; justify-content: center;"
    ] 
    [
      div 
        ~a:[
          a_style "background: white; padding: 30px; border-radius: 15px; 
                   text-align: center; box-shadow: 0 15px 35px rgba(0,0,0,0.7);
                   max-width: 600px; max-height: 80vh; overflow-y: auto;"
        ]
        [
          h1 ~a:[a_style "color: #2c3e50; margin-bottom: 20px; font-size: 32px;"] 
             [txt "🦠 Welcome to H42N42! 🦠"];
          
          h2 ~a:[a_style "color: #e74c3c; margin-bottom: 15px; font-size: 24px;"] 
             [txt "🎯 Your Mission"];
          
          p ~a:[a_style "color: #333; font-size: 16px; margin-bottom: 20px; line-height: 1.5;"] 
            [txt "Protect the Creets from a deadly epidemic! Make them survive as long as you can by using strategy and quick thinking."];
          
          h3 ~a:[a_style "color: #2c3e50; margin-bottom: 15px; font-size: 20px;"] 
             [txt "📖 How to Play"];
          
          div ~a:[a_style "text-align: left; margin-bottom: 20px;"]
          [
            ul ~a:[a_style "color: #555; font-size: 14px; line-height: 1.6; padding-left: 20px;"]
            [
              li [txt "🦠 "; strong [txt "Infection mechanism:"]; txt " A sick Creet in contact with a healthy Creet can contaminate it"];
              li [txt "🖱️ "; strong [txt "Drag & Drop:"]; txt " Click and drag Creets to move them"];
              li [txt "🏥 "; strong [txt "Green Zone (Left):"]; txt " Heal infected Creets by dropping them here"];
              li [txt "☠️ "; strong [txt "Purple Zone (Right):"]; txt " Dangerous infection zone - keep Creets away!"];
              li [txt "✨ "; strong [txt "Immunity:"]; txt " Dragged Creets become temporarily immune"];
              li [txt "🎯 "; strong [txt "Goal:"]; txt " Keep at least one Creet healthy as long as you can"];
            ]
          ];
          
          h3 ~a:[a_style "color: #2c3e50; margin-bottom: 15px; font-size: 20px;"] 
             [txt "🧬 Types of Creets"];
          
          div ~a:[a_style "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; margin-bottom: 25px;"]
          [
            div ~a:[a_style "background: #e8f5e8; padding: 15px; border-radius: 8px; border-left: 4px solid #4CAF50;"]
            [
              p ~a:[a_style "margin: 0; color: #2e7d32; font-weight: bold;"] [txt "😊 Healthy"];
              p ~a:[a_style "margin: 5px 0 0 0; color: #555; font-size: 13px;"] [txt "Normal Creets. Protect them at all costs!"];
            ];
            
            div ~a:[a_style "background: #ffebee; padding: 15px; border-radius: 8px; border-left: 4px solid #F44336;"]
            [
              p ~a:[a_style "margin: 0; color: #c62828; font-weight: bold;"] [txt "🤢 Sick"];
              p ~a:[a_style "margin: 5px 0 0 0; color: #555; font-size: 13px;"] [txt "Infected and contagious. Can spread the disease."];
            ];
            
            div ~a:[a_style "background: #fce4ec; padding: 15px; border-radius: 8px; border-left: 4px solid #8B0000;"]
            [
              p ~a:[a_style "margin: 0; color: #8B0000; font-weight: bold;"] [txt "👹 Berserk"];
              p ~a:[a_style "margin: 5px 0 0 0; color: #555; font-size: 13px;"] [txt "Grows bigger until death. Fast and dangerous!"];
            ];
            
            div ~a:[a_style "background: #fff3e0; padding: 15px; border-radius: 8px; border-left: 4px solid #FF4500;"]
            [
              p ~a:[a_style "margin: 0; color: #e65100; font-weight: bold;"] [txt "😈 Mean"];
              p ~a:[a_style "margin: 5px 0 0 0; color: #555; font-size: 13px;"] [txt "Hunts healthy Creets. Smaller but aggressive."];
            ];
          ];
          
          div ~a:[a_style "background: #e3f2fd; padding: 15px; border-radius: 8px; margin-bottom: 25px;"]
          [
            h4 ~a:[a_style "margin: 0 0 10px 0; color: #1976d2;"] [txt "💡 Pro Tips"];
            ul ~a:[a_style "margin: 0; padding-left: 20px; color: #555; font-size: 13px; text-align: left;"]
            [
              li [txt "Speed increases over time - act fast!"];
              li [txt "Mean Creets chase healthy ones - intercept them!"];
              li [txt "Use the green zone strategically to cure infections"];
              li [txt "Watch out for the purple zone - it spreads infection"];
            ]
          ];
          
          button 
            ~a:[
              a_onclick (fun _ -> start_game (); false);
              a_style "padding: 15px 40px; font-size: 20px; background: linear-gradient(45deg, #4CAF50, #45a049); 
                       color: white; border: none; border-radius: 10px; 
                       cursor: pointer; box-shadow: 0 6px 12px rgba(76,175,80,0.3);
                       transition: all 0.3s ease;"
            ]
            [txt "🚀 Start the Fight!"];
        ]
    ] in
  
  let body = Dom_html.document##.body in
  Dom.appendChild body (of_element overlay)


let restart_game () =
  Game_state.reset_game ();
  
  let overlays = Dom_html.document##querySelectorAll (Js.string "div[style*='position: fixed']") in
  for i = 0 to overlays##.length - 1 do
    match Js.Opt.to_option (overlays##item i) with
    | Some elem -> 
        (match Js.Opt.to_option elem##.parentNode with
         | Some parent -> 
             let elem_as_node = (elem :> Dom.node Js.t) in
             ignore (parent##removeChild elem_as_node)
         | None -> ())
    | None -> ()
  done;
  
  !create_page_callback ()

let show_game_over_popup () =
  let duration = Game_state.get_game_duration () in
  let formatted_time = Game_state.format_time duration in
  let (total, _, _, _, _, _) = Game_state.get_stats () in
  
  let overlay = div 
    ~a:[
      a_style "position: fixed; top: 0; left: 0; width: 100%; height: 100%; 
               background: rgba(0,0,0,0.8); z-index: 1000; display: flex; 
               align-items: center; justify-content: center;"
    ] 
    [
      div 
        ~a:[
          a_style "background: white; padding: 40px; border-radius: 15px; 
                   text-align: center; box-shadow: 0 10px 30px rgba(0,0,0,0.5);
                   max-width: 500px;"
        ]
        [
          h1 ~a:[a_style "color: #d32f2f; margin-bottom: 20px; font-size: 36px;"] 
             [txt "💀 GAME OVER 💀"];
          
          p ~a:[a_style "color: #666; font-size: 18px; margin-bottom: 20px;"] 
            [txt "All Creets have been infected or died!"];
          
          div ~a:[a_style "background: #f5f5f5; padding: 20px; border-radius: 10px; margin-bottom: 25px;"]
          [
            h2 ~a:[a_style "color: #2c3e50; margin-bottom: 15px; font-size: 24px;"] 
               [txt "📊 Final Results"];
            
            div ~a:[a_style "display: grid; grid-template-columns: 1fr 1fr; gap: 10px; margin-bottom: 15px;"]
            [
              p ~a:[a_style "margin: 5px 0; color: #333; font-weight: bold;"] 
                [txt ("⏱️ Survival Time: " ^ formatted_time)];
            ];
            
            p ~a:[a_style "margin: 10px 0 0 0; color: #666; font-size: 14px;"] 
              [txt ("Total Creets: " ^ string_of_int total)];
          ];
          
          p ~a:[a_style "color: #333; font-size: 16px; margin-bottom: 30px; font-style: italic;"] 
            [txt (if duration > 180.0 then "You fought well, you can be proud!"
                  else if duration > 120.0 then "Great job surviving over 2 minutes!" 
                  else if duration > 60.0 then "Good effort! Try to last longer next time."
                  else "Come on bruh, I know you can do better...")];
          
          button 
            ~a:[
              a_onclick (fun _ -> restart_game (); false);
              a_style "padding: 15px 30px; font-size: 18px; background: linear-gradient(45deg, #4CAF50, #45a049); 
                       color: white; border: none; border-radius: 8px; 
                       cursor: pointer; box-shadow: 0 4px 8px rgba(76,175,80,0.3);"
            ]
            [txt "🔄 Fight Again"];
        ]
    ] in
  
  let body = Dom_html.document##.body in
  Dom.appendChild body (of_element overlay)




  