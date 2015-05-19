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

type expr =
  | Single of nameterm
  | Many of nameterm list

and nameterm =
  | Namespace of string * term
  | Term of term

and term =
  | Variable of string
  | Record
  | Macro
  | Deck
  | Content
[@@deriving show]
