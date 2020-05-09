(**
 * Testing refactor modules.
 *)

module Alt =
  struct
    type alt = {
      content : string;
      attributes : (string * string) list
    }
  end

module type TAG =
  sig
    type t
    type tbl = (string, t) Hashtbl.t
  end

module type NAMESPACE =
  functor (Macro : TAG) ->
  sig
    type t = {
      name : string;
      macro_tbl : Macro.tbl;
    }
  end

module MakeNamespace : NAMESPACE =
  functor (T1 : TAG) ->
  struct
    type t = {
      name : string;
      macro_tbl : T1.tbl;
    }
  end

module MacroTag : TAG =
  struct
    type t = {
      name : string;
      alts : string list;
    }
    type tbl = (string, t) Hashtbl.t
  end

module Namespace = MakeNamespace(MacroTag)

module Macro =
  struct
    let eval_macro (name : string) (namespace : Namespace.t) : string =
      if Hashtbl.mem namespace.macro_tbl name then begin
        let macro = Hashtbl.find namespace.macro_tbl name in
        ()
      end;
      ""
  end
