open Js_of_ocaml

class creet = object (self)
  val mutable x = Random.float 1600.0
  val mutable y = Random.float 800.0
  val mutable speed = 15.0
  val mutable diameter = 40.0
  val mutable vx = 
    let angle = Random.float (2.0 *. 3.14159) in
    let speed = 15.0 in
    speed *. cos angle
  val mutable vy = 
    let angle = Random.float (2.0 *. 3.14159) in
    let speed = 15.0 in
    speed *. sin angle
  val mutable status = "healthy"

  method get_x = x
  method get_y = y
  method get_vx = vx
  method get_vy = vy
  method get_status = status
  method get_speed = speed
  method get_diameter = diameter

  method set_x new_x = x <- new_x
  method set_y new_y = y <- new_y
  method set_vx new_vx = vx <- new_vx
  method set_vy new_vy = vy <- new_vy
  method set_status new_status = status <- new_status


  method random_angle =
  let angle = Random.float (2.0 *. 3.14159) in
  vx <- speed *. cos angle;
  vy <- speed *. sin angle
  


  method update dt =
    (* Printf.printf "updating...\n%!"; *)
    x <- x +. vx *. dt;
    if (x +. diameter /. 2.0) >= 1600.0 then begin
      self#set_vx (-.vx); 
      self#set_x (1600.0 -. diameter /. 2.0);
    end;
    if x <= (diameter /. 2.0) then begin
      self#set_vx (-.vx); 
      self#set_x (diameter /. 2.0);
    end;

    y <- y +. vy *. dt;
    if y < (diameter /. 2.0) then begin
      self#set_y (diameter /. 2.0);
      self#set_vy (-.vy);
    end; 
    if (y +. diameter /. 2.0) >= 800.0 then begin
      self#set_y (800.0 -. diameter /. 2.0);
      self#set_vy (-.vy);
    end;
    self#set_status "healthy";

    if Random.int_in_range ~min:0 ~max:200 = 1 then
      self#random_angle;
    speed <- speed +. 0.01;

    (* Printf.printf "x = %f, y = %f %!" self#get_x self#get_y *)



  method draw (ctx : Dom_html.canvasRenderingContext2D Js.t) =
    let color = match status with
      | "actif" -> "blue"
      | "inactif" -> "gray"
      | "danger" -> "red"
      | "mort" -> "black"
      | _ -> "green"
    in
    ctx##.fillStyle := Js.string color;
    
    ctx##beginPath;
    let f = Js.number_of_float in
    let pi = 4.0 *. atan 1.0 in
    ctx##arc (f x) (f y) (f (diameter /. 2.0)) (f 0.0) (f (2.0 *. pi)) Js._true;
    ctx##fill;
    ()
end