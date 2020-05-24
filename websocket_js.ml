(**
 * Compile with:
 *   ocamlfind ocamlc -package js_of_ocaml,js_of_ocaml-ppx -linkpkg -o websocket_js.byte websocket_js.ml
 *   js_of_ocaml websocket_js.byte
 *)
open Js_of_ocaml

let console = Js.Unsafe.global##console
