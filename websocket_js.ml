(**
 * Compile with:
 *   ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml-ppx -linkpkg -o websocket_js.byte websocket_js.ml
 *   js_of_ocaml websocket_js.byte
 *)
open Js_of_ocaml
open Js

let _ =
  Firebug.console##log (string "here");
  let ws : WebSockets.webSocket t = new%js WebSockets.webSocket (string "ws://localhost:8080") in
  (** TODO: Fill in the blanks, Dom.event ect *)
  let onopen : ('a t, 'a Dom.event t) Dom.event_listener = Dom.handler
    (fun s ->
      ws##send (string "asd");
      bool false
    )
  in
  let onmessage : (_, _) Dom.event_listener = Dom.handler
    (fun ev ->
      Firebug.console##log (string ("Message from server: " ^ (to_string ev##.data)));
      bool false
    )
  in
  ws##.onopen    := onopen;
  ws##.onmessage := onmessage;
  ()
