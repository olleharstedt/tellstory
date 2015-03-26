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

%token <bool> BOOL
%token AND OR NOT
%token LPAREN RPAREN
%token EOL

%left AND OR         /* lowest precedence */
%nonassoc NOT        /* highest precedence */

%start <bool> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = BOOL
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr AND e2 = expr
    { e1 && e2 }
| e1 = expr OR e2 = expr
    { e1 || e2 }
| NOT e = expr %prec NOT
    { not e }

