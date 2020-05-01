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

module Make (Tag : TAG) : S with type tag_tbl = Tag.tbl =
  struct
    type state
    type tag_tbl = Tag.tbl
    type namespace = {
      name : string;
      deck_tbl : tag_tbl
    }
    let log_trace _ =()
  end

module My_tag = struct
  type t = int
  type tbl = (string, t) Hashtbl.t

  let eval_deck (name : string) (namespace : namespace) : t =
    Hashtbl.find namespace.deck_tbl deck_name
end

module My_s = Make(My_tag)
