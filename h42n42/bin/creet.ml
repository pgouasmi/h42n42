open Js_of_ocaml
open Js_of_ocaml_tyxml.Tyxml_js.Html
open Js_of_ocaml_tyxml.Tyxml_js.To_dom
open Lwt.Syntax
open Js_of_ocaml_lwt

class creet (initial_speed : float) = object (self)
  val mutable base_speed = initial_speed
  val mutable x = Random.float 1400.0
  val mutable y = Random.float 700.0
  val mutable vx = 
    let angle = Random.float (2.0 *. 3.14159) in
    let speed = initial_speed in
    speed *. cos angle
  val mutable vy = 
    let angle = Random.float (2.0 *. 3.14159) in
    let speed = initial_speed in
    speed *. sin angle
  val mutable speed = initial_speed
  val mutable diameter = 60.0
  val max_diameter = 240.0
  val mutable status = "healthy"
  val mutable dom_element = None
  val mutable is_alive = true
  val mutable is_dragged = false
  val mutable is_immune = false
  val mutable is_draggable = true
  val mutable drag_offset_x = 0.0
  val mutable drag_offset_y = 0.0
  val mutable behavior_thread = None
  val canvas_width = 1600.0
  val canvas_height = 800.0
  val mutable current_speed_modifier = 1.0

  method get_x = x
  method get_y = y
  method get_vx = vx
  method get_vy = vy
  method get_speed = speed
  method get_diameter = diameter
  method get_status = status
  method get_dom_element = dom_element
  method is_alive = is_alive
  method is_dragged = is_dragged
  method is_immune = is_immune

  method set_x new_x = x <- new_x
  method set_y new_y = y <- new_y
  method set_vx new_vx = vx <- new_vx
  method set_vy new_vy = vy <- new_vy
  method set_status new_status = 
    status <- new_status;
    self#update_dom_style ()
  method kill = is_alive <- false
  method set_dragged dragged = is_dragged <- dragged
  method set_immune immune = is_immune <- immune

  method start_drag mouse_x mouse_y =
    if status != "dead" && status != "dead_deflating" then (
      is_dragged <- true;
      is_immune <- true;
      drag_offset_x <- mouse_x -. x;
      drag_offset_y <- mouse_y -. y;
      vx <- 0.0;
      vy <- 0.0;
      self#update_dom_style ()
    )

  method update_drag mouse_x mouse_y =
    if is_dragged then (
      x <- mouse_x -. drag_offset_x;
      y <- mouse_y -. drag_offset_y;
      self#update_dom_position ()
    )

  method stop_drag () =
    if is_dragged then (
      is_dragged <- false;
      is_immune <- false;
      
      if x <= 80.0 && (status = "sick" || status = "berserk" || status = "mean") then (
        self#set_status "healthy"
      );
      
      self#random_angle;
      self#update_dom_style ()
    )

  method get_effective_speed () =
    let status_modifier = match status with
      | "healthy" -> 1.0
      | "sick" -> 0.85
      | "berserk" -> 0.85
      | "mean" -> 0.85
      | "dead" | "dead_deflating" -> 0.0
      | _ -> 1.0
    in
    base_speed *. status_modifier

  method set_global_speed new_speed =
    base_speed <- new_speed;
    self#update_speed ()

  method update_speed () =
    let new_speed = self#get_effective_speed () in
    let current_magnitude = sqrt (vx *. vx +. vy *. vy) in
    if current_magnitude > 0.0 then (
      let direction_x = vx /. current_magnitude in
      let direction_y = vy /. current_magnitude in
      vx <- direction_x *. new_speed;
      vy <- direction_y *. new_speed
    );
    speed <- new_speed

  method create_dom_element (container : Dom_html.element Js.t) =
    let color = match status with
      | "healthy" -> "#4CAF50"
      | "sick" -> "#F44336"
      | "dead" | "dead_deflating" -> "#9E9E9E"
      | "berserk" -> "#8B0000"
      | "mean" -> "#FF4500"
      | _ -> "#2196F3"
    in
    
    let style_str = Printf.sprintf 
      "position: absolute; left: %fpx; top: %fpx; width: %fpx; height: %fpx; 
       background-color: %s; border-radius: 50%%; border: 2px solid #333;
       transition: all 0.2s ease; cursor: pointer; display: flex;
       align-items: center; justify-content: center; font-size: 12px;
       color: white; font-weight: bold; z-index: 2;" 
      x y diameter diameter color in
    
    let emoji = match status with
      | "healthy" -> "😊"
      | "sick" -> "🤢"
      | "dead" | "dead_deflating" -> "💀"
      | "berserk" -> "👹"
      | "mean" -> "😈"
      | _ -> "🙂"
    in
    
    let div_elem = div 
      ~a:[
        a_style style_str;
        a_class ["creet"];
        a_id ("creet_" ^ string_of_int (Random.int 100000))
      ] 
      [txt emoji] in
    
    let dom_div = of_element div_elem in
    dom_element <- Some dom_div;

    dom_div##.onmousedown := Dom_html.handler (fun ev ->
      self#start_drag (Js.to_float ev##.clientX) (Js.to_float ev##.clientY);
      ev##preventDefault;
      Js._true
    );
    
    Dom.appendChild container dom_div;
    dom_div

  method update_dom_position () =
    match dom_element with
    | Some elem ->
        elem##.style##.left := Js.string (string_of_float x ^ "px");
        elem##.style##.top := Js.string (string_of_float y ^ "px")
    | None -> ()

  method update_dom_style () =
    match dom_element with
    | Some elem ->
        let (color, emoji) = match (status, is_dragged) with
          | (_, true) -> ("#FFD700", "✨")
          | ("healthy", _) -> ("#4CAF50", "😊")
          | ("sick", _) -> ("#F44336", "🤢")
          | ("dead", _) | ("dead_deflating", _) -> ("#9E9E9E", "💀")
          | ("berserk", _) -> ("#8B0000", "👹")
          | ("mean", _) -> ("#FF4500", "😈")
          | _ -> ("#2196F3", "🙂")
        in
        elem##.style##.backgroundColor := Js.string color;
        elem##.textContent := Js.some (Js.string emoji)
    | None -> ()

  method update_dom_size () =
    match dom_element with
    | Some elem ->
        elem##.style##.width := Js.string (string_of_float diameter ^ "px");
        elem##.style##.height := Js.string (string_of_float diameter ^ "px")
    | None -> ()

  method remove_from_dom () =
    match dom_element with
    | Some elem ->
        (match Js.Opt.to_option elem##.parentNode with
         | Some parent -> 
             let elem_as_node = (elem :> Dom.node Js.t) in
             ignore (parent##removeChild elem_as_node)
         | None -> ());
        dom_element <- None
    | None -> ()

  method collides_with (other_creet : creet) =
    let dx = x -. other_creet#get_x in
    let dy = y -. other_creet#get_y in
    let distance = sqrt (dx *. dx +. dy *. dy) in
    let min_distance = (diameter +. other_creet#get_diameter) /. 2.0 in
    distance <= min_distance

  method check_infection_zone () =
    let infection_zone_left = canvas_width -. 80.0 in
    let creet_right_edge = x +. diameter in
    
    if creet_right_edge >= infection_zone_left +. 15.0 && status = "healthy" && not is_dragged && not is_immune then (
      let rdm = Random.int 100 in
      if rdm < 10 then
        self#set_status "berserk"
      else if rdm >= 90 then
        self#set_status "mean"
      else
        self#set_status "sick"
    )

  method try_contaminate (other_creet : creet) =
    if is_immune || other_creet#is_immune then false
    else if self#collides_with other_creet && status != "healthy" && status != "dead" && status != "dead_deflating" && other_creet#get_status = "healthy" then (
      let rdm = Random.int 100 in
      if rdm < 2 then (
        let rdm2 = Random.int 100 in
        if rdm2 < 10 then
          other_creet#set_status "berserk"
        else if rdm2 >= 90 then
          other_creet#set_status "mean"
        else
          other_creet#set_status "sick";
        true
      ) else false
    ) else false

  method bounce_check () =
    let mut_changed = ref false in
    
    if x +. diameter >= canvas_width then (
      self#set_x (canvas_width -. diameter);
      self#set_vx (-.vx);
      mut_changed := true
    );
    
    if x <= 0.0 then (
      self#set_x 0.0;
      self#set_vx (-.vx);
      mut_changed := true
    );
    
    if y +. diameter >= canvas_height then (
      self#set_y (canvas_height -. diameter);
      self#set_vy (-.vy);
      mut_changed := true
    );
    
    if y <= 0.0 then (
      self#set_y 0.0;
      self#set_vy (-.vy);
      mut_changed := true
    );
    
    !mut_changed

  method move_step dt =
    if not is_dragged then (
      x <- x +. vx *. dt;
      y <- y +. vy *. dt;
      ignore (self#bounce_check ());
      self#update_dom_position ()
    )

  method autonomous_behavior () =
    match status with
    | "healthy" ->
        self#check_infection_zone ();
        if Random.float 1.0 < 0.05 then (
          let angle_change = (Random.float 0.4) -. 0.2 in
          let current_angle = atan2 vy vx in
          let new_angle = current_angle +. angle_change in
          let current_speed = self#get_effective_speed () in
          vx <- current_speed *. cos new_angle;
          vy <- current_speed *. sin new_angle
        );
        if diameter > 60.0 then (
          diameter <- diameter -. 0.1;
          self#update_dom_size ()
        );
        if diameter < 60.0 then (
          diameter <- diameter +. 0.1;
          self#update_dom_size ()
        )

    | "dead" ->
        vx <- 0.0;
        vy <- 0.0;
        is_alive <- false;
        

    | "berserk" ->
        if diameter <= max_diameter then (
          diameter <- diameter +. 0.1;
          self#update_dom_size ()
        ) else (
          self#set_status "dead_deflating"
        )

    | "dead_deflating" ->
        vx <- 0.0;
        vy <- 0.0;
        if diameter > 60.0 then (
          diameter <- diameter -. 0.1;
          self#update_dom_size ()
        ) else (
          self#set_status "dead";
          is_draggable <- false
        )

    | "mean" -> 
        if diameter > 51.0 then (
          diameter <- 60.0 *. 0.85;
          self#update_dom_size ()
        )
    
    | _ -> ()

  method start_behavior_thread () =
    let rec behavior_loop () =
      if not is_alive then
        Lwt.return_unit
      else
        let dt = 0.03 in
        
        self#move_step dt;
        
        self#autonomous_behavior ();
        
        let* () = Lwt_js.sleep 0.03 in
        behavior_loop ()
    in
    
    let thread = behavior_loop () in
    behavior_thread <- Some thread;
    Lwt.async (fun () -> thread)

  method stop_behavior_thread () =
    match behavior_thread with
    | Some _ -> 
        is_alive <- false;
        behavior_thread <- None
    | None -> ()

  method random_angle =
    let angle = Random.float (2.0 *. 3.14159) in
    let current_speed = self#get_effective_speed () in
    vx <- current_speed *. cos angle;
    vy <- current_speed *. sin angle

  method initialize container =
    ignore (self#create_dom_element container);
    self#start_behavior_thread ()
end