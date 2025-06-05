open Js_of_ocaml
(* open Js_of_ocaml.Console *)
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js.To_dom
(* open Unix *)

let global_creets = ref []

module Canvas = struct
  (* let pi = 4.0 *. atan 1.0 *)
  let f = Js.number_of_float

  let width = 1600.
  let height = 800.
(* 
  let arc ctx x y radius start_angle end_angle =
    ctx##arc (f x) (f y) (f radius) (f start_angle) (f end_angle) Js._true *)
  let clear_rect ctx =
    ctx##clearRect (f 0.) (f 0.) (f width) (f height)
  let fill_rect ctx =
    ctx##fillRect (f 0.) (f 0.) (f width) (f height)
  (* let circle ctx x y radius =
    ctx##beginPath;
    arc ctx x y radius 0.0 (2.0 *. pi);
    ctx##fill *)
end

let get_canvas () =
  Dom_html.getElementById_coerce "game-canvas" Dom_html.CoerceTo.canvas

let get_canvas_context () =
  match get_canvas () with
  | Some canvas -> canvas##getContext (Dom_html._2d_)
  | None -> failwith "Canvas not found"


let init_creets count =
  global_creets := [];  (* vider la liste au cas où *)
  for i = 1 to count do
    let new_creet = new Creet.creet in
    global_creets := new_creet :: !global_creets;
    ignore i
  done

let draw_creets ctx =
  List.iter (fun creet -> creet#draw ctx) !global_creets

let move_creets () =
  List.iter (fun creet -> creet#update 0.1) !global_creets
  

let create_page () =
  Dom_html.document##.title := Js.string "H42N42";
  
  let canvas_elem = canvas 
    ~a:[
      a_id "game-canvas";
      a_width 1600;
      a_height 800;
      a_style "border: 2px solid #333; display: block; margin: 20px auto;"
    ] [] in
  
  let page_content = [
    h1 [txt "Welcome to H42N42"];
    h2 [txt "Protect the Creets at all cost!"];
    canvas_elem;
  ] in
  
  let body = Dom_html.document##.body in
  body##.innerHTML := Js.string "";
  
  List.iter (fun elem -> 
    Dom.appendChild body (of_element elem)
  ) page_content;
  
  (* Dessin sur le canvas *)
  let ctx = get_canvas_context () in
  Canvas.clear_rect ctx;
  ctx##.fillStyle := Js.string "#f0f0f0";
  Canvas.fill_rect ctx;
  init_creets 100;
  draw_creets ctx;


  (* let time = Unix.gettimeofday () in
  ignore time;
  (* Unix.sleepf 3.0; *)
  print_string "slept 3";
  method random_angle =

  for i = 0 to 10000 do 
    move_creets;
    draw_creets ctx;
    ignore i 
  done; *)
  (* draw_creets ctx; *)
  ()

  let rec game_loop () =
    (* Printf.printf "In game loop\n%!"; *)
    let ctx = get_canvas_context () in
  
  (* Clear canvas *)
    Canvas.clear_rect ctx;
    ctx##.fillStyle := Js.string "#f0f0f0";
    Canvas.fill_rect ctx;
    
    (* Update et draw *)
    move_creets (); (* ~60 FPS *)
    draw_creets ctx;
    
    ignore (Dom_html.window##requestAnimationFrame
      (Js.wrap_callback (fun _ -> game_loop ())));
      ()

let () =
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    Random.self_init ();
    create_page () ;
    game_loop () ;
    Js._false
  )