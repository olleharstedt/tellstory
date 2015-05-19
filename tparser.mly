/**************************************************************************/
/*                                                                        */
/*  Menhir                                                                */
/*                                                                        */
/*  François Pottier, INRIA Rocquencourt                                  */
/*  Yann Régis-Gianas, PPS, Université Paris Diderot                      */
/*                                                                        */
/*  Copyright 2005-2008 Institut National de Recherche en Informatique    */
/*  et en Automatique. All rights reserved. This file is distributed      */
/*  under the terms of the Q Public License version 1.0, with the change  */
/*  described in file LICENSE.                                            */
/*                                                                        */
/**************************************************************************/

%{
  open Ast
%}

%token <string> WORD
(*
%token BACKSLASH
*)
%token EOL

%start <Ast.term> main

%%

main:
| e = expr EOL
    { e }

expr:
| w = WORD
    { Variable w }
    (*
| e = term
    { e }
*)

(* Bla bla *)
    (*
nameterm:
| n = WORD BACKSLASH term
    { n }
*)

    (*
term:
| e = WORD
    { Variable e }
*)
