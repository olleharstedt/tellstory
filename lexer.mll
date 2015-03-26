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
  open Parser

  exception Error of string

}

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
| "AND"
    { AND }
| "OR"
    { OR }
| "NOT"
    { NOT }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| ['a'-'z' 'A'-'Z' '_' '0'-'9']+ as i
    { BOOL (Hashtbl.mem Globals.flags_tbl i) }
| eof
    { EOL}
| _
    { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }

