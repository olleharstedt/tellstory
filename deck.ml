(**
 * Module for the <deck> tag
 *)

module type S =
  sig
    type t
    type tbl
    type namespace
    val eval_deck : string -> namespace -> string
  end

module Make (Com : Common.S) =
  struct
    type t = {
      name : string;
    }
    type tbl = (string, t) Hashtbl.t
    type namespace = Com.namespace
    
    let eval_deck (deck_name : string) (namespace : namespace) : string =
      let deck : t = Com.hashtbl_find namespace.deck_tbl deck_name in
      "foo"
  end
