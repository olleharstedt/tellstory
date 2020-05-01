(**
 * AST for template lang
 *
 * @author Olle Härstedt
 * @since 2015-05-19
 *
    terms:
      variable
      record.this
      #macro
      $deck
      @graph
      %dice
      "inline content"
    nameterm:
      namespace\term
      term
    expr:
      nameterm | nameterm
      nameterm
    main:
      expr
 *)

exception Eval_ast_exception of string

(* List like 'oneitem' or 'one|two|three' *)
type nameterm_list =
  | Nameterm_list of nameterm list

(* Either 'var' or 'namespace\var' *)
and nameterm =
  | Nameterm of string * term
  | Term of term
  | Content of string

(* Atom like 'var' or '$deck' *)
and term =
  | Variable of string
  | Record of string * string
  | Macro of string
  | Deck of string
  | Graph of string
  | Dice of string * int
  | Input of string
  | Plus of term * term
[@@deriving show]

(*
module type T = sig
  val eval_ast : nameterm_list -> (int -> int) -> state -> namespace -> string
end
*)
