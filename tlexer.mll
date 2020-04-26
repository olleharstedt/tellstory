(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

{
  open Tparser

  exception Error of string

}

(* OBS: When adding new token, don't forget to add it in reg expr in tellstory.ml *)

rule line = parse
| ([^'\n']* '\n') as line
    { line }
| eof
    { "" }

and token = parse
| [' ' '\t']
    { token lexbuf }
| '\n'
    { EOL }
| '\\'
    { BACKSLASH }
| '$'
    { DECKSIGN }
| '@'
    { GRAPHSIGN }
| '%'
    { DICESIGN }
| '#'
    { MACROSIGN }
| '.'
    { RECORDDOT }
| ':'
    { INPUTSIGN }
    (*
| '"'
    { QUOTE }
*)
| '|'
    { BARLINE }
| ['0'-'9']+ as i
    { INT (int_of_string i) }
| ['a'-'z' 'A'-'Z' '_' '0'-'9']+ as i
    { WORD i }
| ['"'] [^ '"']+ ['"'] as i
    { QUOTE i }
| eof
    { EOL}
| _
    { raise (Error (Printf.sprintf "unexpected character: %s\n" (Lexing.lexeme lexbuf))) }

