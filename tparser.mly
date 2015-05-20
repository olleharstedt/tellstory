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
%token DECKSIGN
%token MACROSIGN
%token RECORDDOT
%token QUOTE
%token BACKSLASH

(*
%token BACKSLASH
*)
%token EOL

%start <Ast.nameterm> main

%%

main:
| e = nameterm EOL
    { e }

nameterm:
| n = term
    { Term n }
| w = WORD BACKSLASH n = term
    { Nameterm (w, n) }

term:
| w = WORD
    { Variable w }
| DECKSIGN w = WORD
    { Deck w }
| MACROSIGN w = WORD
    { Macro w }
| w = WORD RECORDDOT u = WORD
    { Record (w, u) }
| QUOTE w = WORD QUOTE
    { Content w }
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
