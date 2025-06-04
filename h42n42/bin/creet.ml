open Js_of_ocaml

class creet = object (self)
  val mutable x = Random.float 1600.0  (* Position X aléatoire *)
  val mutable y = Random.float 800.0  (* Position Y aléatoire *)
  val mutable speed = 50
  val mutable diameter = 50
  val mutable vx = 
    let angle = Random.float (2.0 *. 3.14159) in
    let speed = 50.0 in  (* Vitesse constante *)
    speed *. cos angle
  val mutable vy = 
    let angle = Random.float (2.0 *. 3.14159) in
    let speed = 50.0 in  (* Vitesse constante *)
    speed *. sin angle
  val mutable status = "healthy"

  (* Getters *)
  method get_x = x
  method get_y = y
  method get_vx = vx
  method get_vy = vy
  method get_status = status
  method get_speed = speed
  method get_diameter = diameter

  (* Setters *)
  method set_x new_x = x <- new_x
  method set_y new_y = y <- new_y
  method set_vx new_vx = vx <- new_vx
  method set_vy new_vy = vy <- new_vy
  method set_status new_status = status <- new_status
  


  (* Méthode de mise à jour *)
  method update dt =
    x <- x +. vx *. dt;
    y <- y +. vy *. dt;
    self#set_status


  method draw (ctx : Dom_html.canvasRenderingContext2D Js.t) =
    (* Définir la couleur en fonction du statut *)
    let color = match status with
      | "actif" -> "blue"
      | "inactif" -> "gray"
      | "danger" -> "red"
      | "mort" -> "black"
      | _ -> "green"
    in
    ctx##.fillStyle := Js.string color;
    
    (* Dessiner le cercle à la position du creet *)
    ctx##beginPath;
    let f = Js.number_of_float in
    let pi = 4.0 *. atan 1.0 in
    ctx##arc (f x) (f y) (f 15.0) (f 0.0) (f (2.0 *. pi)) Js._true;
    ctx##fill;
    ()
end