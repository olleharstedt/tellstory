(**
 * Testing refactor modules.
 *)

open Printf

module Alt =
  struct
    exception Alt_exception of string

    type t = {
      content : string;
      attributes : (string * string) list
    }

    (**
     * Parse macro/deck xml alts as alt list
     *
     * @param alts Xml.Element list
     * @return alt list
     *)
    let parse_alts (alts : Xml.xml list) : t list =
      List.map (fun alt -> match alt with
        | Xml.Element ("alt", attributes, [Xml.PCData content]) ->
            {content; attributes}
        | Xml.Element ("alt", attributes, []) ->
            {content = ""; attributes}
        | _ ->
            raise (Alt_exception "Illegal alt in macro/deck")
      ) alts 

  end

module Common =
  struct
    let log_trace (msg : string) : unit =
      ()
    (**
     * Aux function to find attribute
     * Find first attribute with name
     *
     * @param attributes ? Xml.Element list
     * @param name string
     * @return Xml.Element option
     *)
    let find_attribute attributes name =
      match List.find (function
        | (attr_name, what) ->
            log_trace (sprintf "looking for attribute '%s', '%s'" attr_name what);
            name = attr_name
      ) attributes with
      | xml -> Some xml
      | exception Not_found -> None
  end

module type TAG =
  sig
    type t
    type tbl = (string, t) Hashtbl.t
  end

module type NAMESPACE =
  functor (Macro : TAG) (Var : TAG) ->
  sig
    type t = {
      name      : string;
      var_tbl   : Var.tbl;
      macro_tbl : Macro.tbl;
    }
    type tbl
    type state
    val get_namespace_or_create : state -> string -> t
  end

module MakeNamespace : NAMESPACE =
  functor (T1 : TAG) (T2 : TAG) ->
  struct
    type t = {
      name      : string;
      var_tbl   : T2.tbl;
      macro_tbl : T1.tbl;
    }
    type tbl = (string, t) Hashtbl.t
    type state = {
      config        : (string * string) list;
      namespace_tbl : tbl;
    }

    let new_namespace name =
      {
        name;
        var_tbl    = ((Hashtbl.create 20) : T2.tbl);
        macro_tbl  = ((Hashtbl.create 20) : T1.tbl);
      }
    (**
     * Get namespace with name @name, or creates a new one
     * with this name if no one is present
     *
     * @param state
     * @param name string
     * @return namespace
     *)
    let get_namespace_or_create state name =
      if Hashtbl.mem state.namespace_tbl name then
        Hashtbl.find state.namespace_tbl name
      else begin
        Common.log_trace (sprintf "new namespace created with name '%s'" name);
        let new_namespace = new_namespace name in
        Hashtbl.add state.namespace_tbl name new_namespace;
        new_namespace
      end

  end

module MacroTag :
  sig
    include TAG
    val create : string -> Alt.t list -> t
  end =
  struct
    type t = {
      name : string;
      alts : Alt.t list;
    }
    type tbl = (string, t) Hashtbl.t
    let create name alts = {name; alts}
  end

module VarTag :
  sig
    include TAG
  end =
  struct
    type t = string
    type tbl = (string, t) Hashtbl.t
  end

module Namespace = MakeNamespace(MacroTag)(VarTag)

exception Internal_error of string

let test = MacroTag.create "test" []

module Macro =
  struct
    exception Macro_exception of string
    (**
     * Parse macro tag
     *
     * @param attrs Xml.xml list
     * @param alts Xml.xml list
     * @param state
     * @return (macro * namespace option)
     *)
    let parse_macro (attrs : (string * string) list) (alts : Xml.xml list) (state : Namespace.state) : (MacroTag.t * Namespace.t option) =
      let namespace_name = Common.find_attribute attrs "namespace" in
      let macro_name = Common.find_attribute attrs "name" in
      begin match macro_name, namespace_name with
      | Some ("name", macro_name), Some ("namespace", namespace_name) ->
          let alts = Alt.parse_alts alts in
          let namespace = Namespace.get_namespace_or_create state namespace_name in
          (MacroTag.create macro_name alts, Some namespace)
      | Some ("name", macro_name), None ->
          let alts = Alt.parse_alts alts in
          MacroTag.create macro_name alts, None
      | None, _ ->
          raise (Macro_exception "Macro has no name attribute")
      | _, _ ->
          raise (Internal_error "Macro definition is super weird")
      end

    (**
     *)
    let eval_macro (name : string) (namespace : Namespace.t) : string =
      if Hashtbl.mem namespace.macro_tbl name then begin
        let macro = Hashtbl.find namespace.macro_tbl name in
        ()
      end;
      ""
  end
