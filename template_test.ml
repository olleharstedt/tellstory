exception Internal_error of string

open Printf

let () =
  let match_ = "abc" in
  let linebuf = Sedlexing.Utf8.from_string match_ in
  let ast = try (Tparser.main Tlexer.token linebuf) with
  (*
    | Tlexer.Error msg ->
        raise (Parser_error (sprintf "inline: Lexer error %s" msg))
*)
  (*
    | Tparser.Error ->
        raise (Parser_error (sprintf "inline: Syntax error at offset %d" (Sedlexing.lexeme_start linebuf)))
*)
    | Failure msg ->
        raise (Internal_error "bla")
        (*
        let open Sedlexing in
        raise (Internal_error (sprintf "line = %d; col = %d" linebuf.lex_curr_p.pos_lnum linebuf.lex_curr_p.pos_cnum))
*)
  in
  ()
