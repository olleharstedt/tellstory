(**
 * Common datastructures and functions for all tag modules.
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

module Make (Tag : TAG) =
  struct 
    type t = Tag.t
    type tbl = Tag.tbl

    type namespace = {
      name : string;
      deck_tbl : tbl;
    }

    type namespace_tbl = (string, namespace) Hashtbl.t

    type state = {
      namespace_tbl : namespace_tbl;
    }

    let hashtbl_find (tbl : Tag.tbl) (name : string) =
      Hashtbl.find tbl name

    (**
     * Debug with Bolt
     *
     * Run with BOLT_FILE=bolt.config ./tellstory
     *
     *)
    let log_trace (msg : string) : unit =
      ()
      (*print_endline ("\ttrace: " ^ msg)*)
      (*Bolt.Logger.log "tellstory_debug_logger" Bolt.Level.TRACE msg*)
  end
