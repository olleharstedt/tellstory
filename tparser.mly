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

(* OBS: When adding new token, don't forget to add it in reg expr in tellstory.ml *)

%token <string> WORD
%token <int> INT
%token DECKSIGN
%token GRAPHSIGN
%token DICESIGN
%token MACROSIGN
%token RECORDDOT
%token <string> QUOTE
%token BACKSLASH
%token BARLINE
%token INPUTSIGN
%token PLUS

(*
%token BACKSLASH
*)
%token EOL

%start <Ast.nameterm_list> main

%%

main:
| e = exp_list EOL
    { e }

exp_list:
| e = separated_list(BARLINE, nameterm)
    { Nameterm_list e }

nameterm:
| n = term
    { Term n }
| w = WORD BACKSLASH n = term
    { Nameterm (w, n) }
| w = QUOTE
    { Content w }

term:
| w = WORD
    { Variable w }
| DECKSIGN w = WORD
    { Deck w }
| GRAPHSIGN w = WORD
    { Graph w }
| MACROSIGN w = WORD
    { Macro w }
| INPUTSIGN w = WORD
    { Input w }
| DICESIGN w  = WORD RECORDDOT i = INT
    { Dice (w, i) }
| DICESIGN w = WORD 
    { Dice (w, 1) }
| i = INT
    { Int i }
| w = WORD RECORDDOT u = WORD
    { Record (w, u) }
| w = term PLUS u = term
    { Plus (w, u) }



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
