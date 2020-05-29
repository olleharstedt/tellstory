type test = One | Two of int | Three of string
[@@deriving json]
type fest = One | Two of int | Three of float
[@@deriving json]

(**
 * ocamlfind opt -package js_of_ocaml-ppx_deriving_json -linkpkg testtype.ml
 *)
let _ =
  let value : test = Two 2 in
  let buffer : Buffer.t = Buffer.create 255 in
  test_to_json buffer value;
  let content : string = Buffer.contents buffer in
  print_endline content;

  let value : fest = Two 2 in
  let buffer : Buffer.t = Buffer.create 255 in
  fest_to_json buffer value;
  let content2 : string = Buffer.contents buffer in
  print_endline content2;

  (*
  let buf : Lexing.lexbuf = Lexing.from_string (Buffer.contents buffer) in
  let value : t = of_json (Deriving_Json_lexer.init_lexer buf) in
  *)
  let value : test =
       content2
    |> Lexing.from_string
    |> Deriving_Json_lexer.init_lexer
    |> test_of_json
  in
  match value with
  | One -> print_endline "One!"
  | Two i -> print_endline ("Number: " ^ string_of_int i)
  | Three s -> print_endline ("String: " ^ s)
