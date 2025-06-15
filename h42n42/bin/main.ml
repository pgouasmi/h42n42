
open Js_of_ocaml

let () = Random.self_init ()

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Game_events.set_create_page_callback Ui.create_page;
    
    Game_events.show_intro_popup ();
    Js._false
  )