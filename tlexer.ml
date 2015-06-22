(**
 * Using sedlex instead of ocamllex for unicode support in inline content
 *
 * @since 2015-06-15
 * @author Olle Harstedt
 *)

(*
open Ulexing
open Tparser

let regexp tab = ['\t' '\x0b']
let regexp wsp = [' ''\t']
*)

(*
ocamlfind ocamlopt -c -package sedlex -linkpkg tlexer.ml
ocamlfind ocamlopt -o template_test -package sedlex -linkpkg tparser.cmx tlexer.cmx template_test.ml
ocamlfind ocamlopt -package sedlex,menhirLib  tparser.cmx sedlex_menhir.ml -c
*)

open Tparser
open Sedlex_menhir

let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]

let rec token buf =
  let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z'] in
  match%sedlex buf with
  | letter -> token buf
  | eof -> DECKSIGN
  | _ -> failwith "Unexpected character"
