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
    type t
    type tbl
    type namespace = {
      name : string;
      deck_tbl : tbl;
    }

    val hashtbl_find : tbl -> string -> t
    val log_trace : string -> unit
  end

module Make (Tag : TAG) : S with type tbl = Tag.tbl
