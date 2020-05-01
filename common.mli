(**
 * This structure is implemented by all tag modules.
 *)
module type TAG =
  sig
    type t
    type tbl = (string, t) Hashtbl.t
  end

module type S =
  sig
    type state
    type tag_tbl
    type namespace = {
      name : string;
      deck_tbl : tag_tbl
    }
    val log_trace : string -> unit
  end

module Make (Tag : TAG) : S with type tag_tbl = Tag.tbl
