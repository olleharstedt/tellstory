module type TAG =
  sig
    type t
    type tbl = (string, t) Hashtbl.t
  end

module type S =
  sig
    type t
    type tbl
  end

module Make (Tag : TAG) : S with type tbl = Tag.tbl
