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

(* List like 'oneitem' or 'one|two|three' *)
type expr =
  | Expr of nameterm list

(* Either 'var' or 'namespace\var' *)
and nameterm =
  | Nameterm of string * term
  | Term of term

(* Atom like 'var' or '$deck' *)
and term =
  | Variable of string
  | Record of string * string
  | Macro of string
  | Deck of string
  | Content of string
[@@deriving show]
