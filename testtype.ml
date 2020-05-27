type t = One | Two of int | Three of string
[@@deriving json]

(**
 * ocamlfind opt -package js_of_ocaml-ppx_deriving_json -linkpkg testtype.ml
 *)
let _ =
  let value : t = Three "foo" in
  let buffer : Buffer.t = Buffer.create 255 in
  to_json buffer value;
  let content : string = Buffer.contents buffer in
  print_endline content;
  let buf : Lexing.lexbuf = Lexing.from_string (Buffer.contents buffer) in
  let value : t = of_json (Deriving_Json_lexer.init_lexer buf) in
  match value with
  | One -> print_endline "One!"
  | Two i -> print_endline ("Number: " ^ string_of_int i)
  | Three s -> print_endline ("String: " ^ s)
