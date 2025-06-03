open Js_of_ocaml

let create_page () =
  Dom_html.document##.title := Js.string "Mon App OCaml";
  
  let body = Dom_html.document##.body in
  
  body##.innerHTML := Js.string "";
  
  let h1 = Dom_html.document##createElement (Js.string "h1") in
  h1##.textContent := Js.some (Js.string "App créée avec dune init!");
  Dom.appendChild body h1;
  
  let p = Dom_html.document##createElement (Js.string "p") in
  p##.textContent := Js.some (Js.string "Setup entièrement automatisé");
  Dom.appendChild body p;
  
  let button = Dom_html.createButton Dom_html.document in
  button##.textContent := Js.some (Js.string "Test OCaml → JS");
  button##.onclick := Dom_html.handler (fun _ ->
    Firebug.console##log (Js.string "Ça marche!");
    let alert_msg = "OCaml fonctionne dans le navigateur!" in
    Dom_html.window##alert (Js.string alert_msg);
    Js._false
  );
  Dom.appendChild body button

let () = 
  Dom_html.window##.onload := Dom_html.handler (fun _ ->
    create_page ();
    Js._false
  )