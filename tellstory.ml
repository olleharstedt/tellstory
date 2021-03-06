(*
 * Render random texts from XML-file.
 *
 * @since 2015-03-05
 * @author Olle Härstedt
 *)

open Printf
(*open Core.List*)

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

let log_debug msg =
    ()
  (*Bolt.Logger.log "tellstory_debug_logger" Bolt.Level.DEBUG msg*)

(**
 * Need to factor out dice function because in tests we want to control
 * return values.
 *
 * @since 2015-03-26
 *)
module type DICE = sig
  val dice : int -> int
end

module type IO = sig
  (* Broadcast to all channels in websocket implementation *)
  val print_string : string -> unit
  val print_endline : string -> unit
  val input_line : in_channel -> string
  (*val input_line_player : in_channel -> int -> string*)
  val flush : unit -> unit
end

(**
 * Module type for Tellstory module produced by functor Make
 *)
module type T = sig
  exception Sentence_problem of string * string

  type state
  type namespace

  val fetch_node : Xml.xml -> string -> Xml.xml
  val print_sentences : Xml.xml -> state -> namespace -> string
  val file_to_string : string -> state -> string
  val story_to_string : Xml.xml -> state -> string
  val init_state : unit -> state
  val get_global_namespace : state -> namespace
  val get_namespace : state -> string -> namespace option

  (* Functions needed by eval_ast module *)
  (* TODO: ? *)
  (*val eval_deck : string -> state -> namespace -> string*)
end

module Make(Dice : DICE) (Io : IO) : T = struct
  exception No_node_content of string
  exception Not_implemented
  exception Internal_error of string
  exception State_exception of string
  exception No_such_lang of string
  exception No_such_direct_node of string
  exception Too_many_attributes of string
  exception Flag_already_set of string
  exception Illegal_attribute_name of string
  exception Error_parsing_xml
  exception Unknown_tag of string
  exception Sentence_problem of string * string (* Message, string of exn *)
  exception Dice_exception of string
  exception Variable_exception of string
  exception Record_exception of string
  exception Alt_exception of string
  exception Parser_error of string
  exception Deck_exception of string
  exception Graph_exception of string
  exception Macro_exception of string
  exception Xml_exception of string
  exception Include_exception of string * exn
  exception Namespace_exception of string
  exception Clear_exception of string
  exception Loop_exception of string
  exception If_exception of string
  exception Tag_exception of string
  exception Ast_exception of string
  exception End_loop

  let rec string_of_exn ex = match ex with
  | No_node_content str -> sprintf "No node content for node %s" str
  | Not_implemented -> "Not implemented"
  | Internal_error str -> sprintf "Internal error: %s" str
  | No_such_lang str -> sprintf "No such language '%s'" str
  | No_such_direct_node str -> sprintf "No such direct node '%s'" str
  | Too_many_attributes str -> sprintf "Too many attributes: %s" str
  | Flag_already_set str -> sprintf "Flag '%s' already set" str
  | Illegal_attribute_name str -> sprintf "Illegal attribute name '%s'" str
  | Error_parsing_xml -> "Error parsing xml"
  | Unknown_tag str -> sprintf "Unknown tag '%s'" str
  | Sentence_problem (sen, str) -> sprintf "Sentence problem for '%s': %s" sen str
  | Macro_exception str -> sprintf "Macro exception for '%s'" str
  | Variable_exception str -> sprintf "Variable exception for '%s'" str
  | Record_exception str -> sprintf "Record exception for '%s'" str
  | Alt_exception str -> sprintf "Alt exception '%s'" str
  | Deck_exception str -> sprintf "Deck exception '%s'" str
  | Parser_error str -> sprintf "Parser error: '%s'" str
  | Xml_exception str -> sprintf "Xml exception: '%s'" str
  | Namespace_exception str -> sprintf "Namespace exception: '%s'" str
  | Include_exception (str, ex) -> sprintf "Include exception: %s: '%s'" str (string_of_exn ex)
  | Loop_exception str -> sprintf "Loop exception: %s" str
  | ex -> raise ex

  (** Data types for storing macros *)
  type alt = {
    content : string;
    attributes : (string * string) list
  }

  type macro = {
    name : string;
    alts : alt list
  }

  (** Types for storing records *)
  type record = {
    name   : string option;
    values : (string, string) Hashtbl.t
  }

  (**
   * NB: <card> is also a synonym to <record> tag.
   * This type is about allowing different tags inside a deck.
   *)
  type card =
    | Alt of alt
    | Record of record

  (** Data types for storing decks *)
  type deck = {
    name : string;
    cards : card list;  (* When an alt is chosen it's removed from alts and put in trash. *)
    trash : card list;
    shuffle_on_empty : bool;
  }

  (** Graph node datatype *)
  type graph_node = {
    id : int;
    content : string option;
    (** List of nodes this node is connected to *)
    (*connections : int list;*)
    connections : string;
    set_flag : string option;
    children : Xml.xml list;
  }

  (** Graph datatype *)
  type graph = {
    name : string;
    nodes : graph_node list;
    current_node : int;
    start_node : int option;
  }

  (** Type for storing <dice> tags *)
  type dice = {
    name : string;
    sides : int;
  }

  (* Item for <list> *)
  type list_item =
    | Alt of alt
    | Macro of macro
    | Deck of deck
    | Graph of graph
    | Record of record
    | Dice of dice

  (* <list> tag *)
  type list_ = {
    name      : string;
    list_type : string;
    items     : list_item list;
  }

  type macro_tbl = (string, macro) Hashtbl.t
  type var_tbl = (string, string) Hashtbl.t
  type deck_tbl = (string, deck) Hashtbl.t
  type graph_tbl = (string, graph) Hashtbl.t
  type record_tbl = (string, record) Hashtbl.t
  type dice_tbl = (string, dice) Hashtbl.t
  type list_tbl = (string, list_) Hashtbl.t

  (** Namespace of macros, variables, ... *)
  type namespace = {
    name       : string;
    macro_tbl  : macro_tbl;
    var_tbl    : var_tbl;
    record_tbl : record_tbl;
    deck_tbl   : deck_tbl;
    graph_tbl  : graph_tbl;
    dice_tbl   : dice_tbl;
    list_tbl   : list_tbl;
  }

  type namespace_tbl = (string, namespace) Hashtbl.t

  (** Explicit state, pass around everywhere... *)
  (** TODO: Include default namespace? *)
  type state = {
    config        : (string * string) list;
    namespace_tbl : namespace_tbl;
  }

  (* Want to construct Ast_eval.Make(this) - how?
  let module Ast_eval = module Ast_eval.Make
  *)

  (** List of attributes allowed in <sentence> tag *)
  let allowed_sentence_attributes = ((Hashtbl.create 5) : ((string, bool) Hashtbl.t))
  let _ =
    Hashtbl.add allowed_sentence_attributes "ifSet" true

  (** List of attributes allowed in <alt> tag *)
  let allowed_alt_attributes = ((Hashtbl.create 5) : ((string, bool) Hashtbl.t))
  let _ =
    Hashtbl.add allowed_alt_attributes "setFlag" true

  (**
   * Get option value
   *
   * @param var 'a option
   * @param alt 'a
   * @return alt if var is None, otherwise 'a
   *)
  let get_opt var alt = match var with
    | None -> alt
    | Some a -> a

  (**
   * Compare if two list of attribues for alt record are equal
   *
   * @param attrs1 (string * string) list
   * @param attrs2 (string * string) list
   * @return bool
   *)
  let rec compare_alt_attributes attrs1 attrs2 = match attrs1, attrs2 with
  | [], [] -> true
  | x, [] -> false
  | [], y -> false
  | (name1, content1)::xs, (name2, content2)::ys ->
      name1 = name2 && content1 = content2 && (compare_alt_attributes xs ys)

  (**
   * Compare two alt records, return true if they're equal
   *
   * @param a1 alt
   * @param a2 alt
   * @return bool true if the two alts are equal, otherwise false
   *)
  let compare_alt alt1 alt2 = match alt1, alt2 with
  | {content = content1; attributes = attrs1}, {content = content2; attributes = attrs2} ->
      content1 = content2 && (compare_alt_attributes attrs1 attrs2)

  (**
   * @param deck
   * @return unit
   * @todo Only for debug/trace?
   *)
  let print_deck (deck : deck) =
    log_trace (sprintf "%s\n" deck.name);
    log_trace "alts:\n";
    List.iter (fun (card : card) : unit ->
      match card with
        | Alt alt ->
          if List.length alt.attributes > 1 then raise (Alt_exception "More then 1 attributes for alt: can't print");
          (* TODO: Throws exception *)
          log_trace (sprintf "<alt>%s</alt>" alt.content);
          begin match List.nth alt.attributes 0 with
            | exception Failure _ ->
              log_trace (sprintf "<alt>%s</alt>" alt.content)
            | attr ->
              log_trace (sprintf "<alt %s='%s'>%s</alt>\n" (fst attr) (snd attr) alt.content)
          end;
        | Record record ->
          raise (Tag_exception "Record card not implemented")
    )
    deck.cards

  (**
   * Fetch node which tag name = tag_name
   *
   * @param xml Xml.t
   * @param node string
   * @return Xml.Element
   *)
  let fetch_node xml tag_name =

    (**
     * Search xml_list for tag
     *
     * @param xml_list Xml.xml list
     * @param tag_name string
     * @return Xml.Element
     *)
    let rec search_xml_list xml_list tag_name = match xml_list with
        [] -> raise Not_found
      | x::xs -> match x with
          Xml.Element (tag, _, _) ->
            if tag = tag_name then x else search_xml_list xs tag_name
        | Xml.PCData _ -> search_xml_list xs tag_name
    in
    match xml with
      Xml.Element (tag, attrs, []) -> raise (Xml_exception (sprintf "No node children for node with tag '%s'?" tag))
    | Xml.Element (tag, attrs, children) ->
        (* TODO: iter depth and bredth *)
        if tag = tag_name then xml else search_xml_list children tag_name
    | Xml.PCData text -> assert false

  (**
   * Fetch the direct child nodes of xml with tag name
   *)
  let fetch_nodes xml tag =
    match xml with
      | Xml.Element (tag, _, nodes) -> nodes
      | _ -> raise (No_such_direct_node tag)

  (**
   * Fetch all xml children of xml, regardless of tag name
   *
   * @param xml Xml.xml
   * @return xml list option
   *)
  let fetch_children xml =
    match xml with
    | Xml.Element (tag, _, nodes) -> nodes
    | Xml.PCData _ -> []

  (**
   * Fetch the text content of a node, like
   * <example>bla</example>
   *
   * @param xml Xml.Element
   * @return string
   * @raise No_node_content if @xml is not last children in list
   *)
  let fetch_content xml = match xml with
      Xml.PCData s -> s
    | Xml.Element (_, _, Xml.PCData text :: _) -> text
    | Xml.Element (tag, _, _) -> "" (*raise (Xml_exception "fetch_content: unknown tag")*)
    (*| Xml.Element (tag, _, _) -> raise (No_node_content tag)*)

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

  (**
   * Return global namespace in state
   *
   * @param state
   * @return namespace
   *)
  let get_global_namespace state =
    Hashtbl.find state.namespace_tbl "global"

  (**
   * Get namespace with name from state
   * If namespace does not exists, it will be created
   *
   * @param state
   * @param name string
   * @return namespace option
   *)
  let get_namespace state name : namespace option =
    if Hashtbl.mem state.namespace_tbl name then
      Some (Hashtbl.find state.namespace_tbl name)
    else
      None

  (**
   * Creates a new empty namespace
   *
   * @param name string Name of namespace
   * @return namespace
   *)
  let new_namespace name =
    {
      name;
      var_tbl    = ((Hashtbl.create 20) : var_tbl);
      macro_tbl  = ((Hashtbl.create 20) : macro_tbl);
      record_tbl = ((Hashtbl.create 20) : record_tbl);
      deck_tbl   = ((Hashtbl.create 20) : deck_tbl);
      graph_tbl  = ((Hashtbl.create 20) : graph_tbl);
      dice_tbl   = ((Hashtbl.create 20) : dice_tbl);
      list_tbl   = ((Hashtbl.create 20) : list_tbl);
    }

  (**
   * Creates a new namespace with name and adds it to the state
   * Raises exception if namespace already exists
   *
   * @param state state
   * @param name string
   * @return namespace
   *)
  let create_namespace state name =
    if Hashtbl.mem state.namespace_tbl name then
      raise (Namespace_exception (sprintf "Namespace with name '%s' already exists in this state" name))
    else begin
      log_debug (sprintf "new namespace created with name '%s'" name);
      let new_namespace = new_namespace name in
      Hashtbl.add state.namespace_tbl name new_namespace;
      new_namespace
    end

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
      log_trace (sprintf "new namespace created with name '%s'" name);
      let new_namespace = new_namespace name in
      Hashtbl.add state.namespace_tbl name new_namespace;
      new_namespace
    end

  (**
   * Returns true if config with $name is on in $state.
   *
   * @param state state
   * @param name string
   * @return bool
   *)
  let config_is_on (state : state) (name : string) : bool =
    let conf : (string * string) option = List.find_opt
      (fun i -> match i with | n, v -> name = n)
      state.config
    in
    match conf with
      | Some (n, v) -> true
      | None -> false

  (**
   * @param string expr
   * @return boolean
   *)
  let eval_flag_expr (expr : string) =
    let lexing = Lexing.from_string expr in
    let flags_ok = try ((Bparser.main Blexer.token) lexing) with
      | Blexer.Error msg ->
          raise (Parser_error (sprintf "ifSet: Lexer error %s" msg))
      | Bparser.Error ->
          raise (Parser_error (sprintf "ifSet: Syntax error at offset %d" (Lexing.lexeme_start lexing)))
      | Failure msg ->
          let open Lexing in
          raise (Internal_error (sprintf "line = %d; col = %d" lexing.lex_curr_p.pos_lnum lexing.lex_curr_p.pos_cnum))
    in
    flags_ok

  (**
   * Parses ifSet="" and return the result
   * Boolean lang
   *
   * @param attrs Xml.Element list
   * @return bool
   * @raise Errors if parse failes
   *)
  let flags_is_ok attrs =
    log_trace "flags_is_ok";
    match find_attribute attrs "ifSet" with
    | None ->
        log_trace "ifSet None";
        true
    | Some ("ifSet", flags) ->
        log_trace (sprintf "ifSet Some, flags: %s" flags);
        eval_flag_expr flags
    | _ ->
        raise (Internal_error "flags_is_ok: Weird result from find_attribute")


  (**
   * Get list of possible alts to consider, conserning flags
   *
   * @param alts Xml.Element list
   * @return Xml.Element list
   *)
  let get_possible_xml_alts alts = match alts with
    | [] ->
        []
    | alts ->
        (* Filter alts that don't belong *)
        let alts2 = List.filter (fun alt ->
          (* Have to check for flag condition to rule out some alts *)
          let attrs = Xml.attribs alt in
          flags_is_ok attrs
          (*
          let ifSet = find_attribute attrs "ifSet" in
          (* Did we find ifSet? *)
          begin match ifSet with
            | None ->
                (* No flag condition? Ey ok. *)
                true
            | Some ("ifSet", flags) ->

                let split_flags = (Str.split (Str.regexp "[ \t]+") flags) in

                (* Check so all flags are set in hash table. *)
                (* TODO: This should use boolean lang *)
                for_all split_flags ~f:(fun flag ->
                  Hashtbl.mem Globals.flags_tbl flag
                )
            | _ ->
                raise (Internal_error "get_possible_xml_alts: Illegal struct of ifSet")
          end
          *)
        ) alts in
        (*(iter alts2 ~f:(fun xml -> log_trace "alt = %s\n" (Xml.to_string xml)));*)
        alts2

  (**
   * As get_possible_xml_alts, but with deck alts.
   *)
  let get_possible_alts alts = match alts with
    | [] -> []
    | alt ->
        let alts2 = List.filter (fun alt ->
          flags_is_ok alt.attributes
        ) alts in
        alts2

  (**
   * @param cards
   * @return card list
   *)
  let get_possible_cards (cards : card list) : card list = 
    match cards with
      | [] -> []
      | _ ->
          List.filter
          (fun (card : card) : bool -> match card with
            | Alt alt -> flags_is_ok alt.attributes
            | _ -> true
          )
          cards


  (**
   * Chose an alt to use, depending on flags
   *
   * @param alts Xml.Element list
   * @return Xml.Element
   * @raise exception
   *)
  let choose_alt alts =
    let possible_alts = get_possible_xml_alts alts in
    (* Debug info
    iter possible_alts ~f:(fun alt ->
      match Xml.children alt with
      | [Xml.PCData cont] -> log_trace "alt content = %s\n" cont
      | [] -> log_trace "alt content empty\n"
      | _ -> assert false
    );
    *)
    let nr = (List.length possible_alts) in
    if nr = 0 then
      (* TODO: Log debug info here? *)
      raise (Alt_exception "No possible alts to choose.")
    else
      List.nth_opt possible_alts (Dice.dice nr)

  (**
   * Choose one of the alt:s in a sentence.
   *
   * @param sentence Xml.xml list, first element is actual sentence, rest is alt:s
   * @return xml option
   *)
  let choose_alt_from_sentence sentence = match sentence with
    | Xml.PCData _ :: [] ->
        None
    | Xml.PCData _ :: alt :: []->
        Some alt
    | Xml.PCData _ :: alts
    | alts ->
        choose_alt alts

  (**
   * Check if alt has a setFlag attributes and return them as string list option
   *
   * @param alt XML
   * @return string list option
   *)
  let get_flags (alt : Xml.xml) : string list option =
    let flags = match alt with
      | Xml.Element (_, ["setFlag", str], _) ->  Some str
      (*| Xml.Element (_, [attr, str], _) ->  raise (Illegal_attribute_name attr)*)
      | Xml.Element (_, x::xs, [Xml.PCData alt_content]) -> raise (Too_many_attributes ("alt: " ^ alt_content))
      | _ -> None
    in
    log_trace (sprintf "get_flags: flags: '%s'" (match flags with | None -> "" | Some flags -> flags));
    (* Split flag into list *)
    let flags_list = match flags with
      | None -> None
      | Some fs -> Some (Str.split (Str.regexp "[ \t]+") fs)
    in
    flags_list

  (**
   * Store flags in flags hash map
   *
   * @param flags_list string list option
   * @return unit
   *)
  let store_flags flags_list = 
    log_trace "store_flags";
    match flags_list with
    | None -> 
        log_trace "store no flag, list empty";
        ()
    | Some fl ->
        log_trace (sprintf "store %d flags" (List.length fl));
      List.iter (fun s ->
        (* Check if flag is already set. If so, abort *)
        if Hashtbl.mem Globals.flags_tbl s then
          raise (Flag_already_set s)
        else begin
          log_debug (sprintf "set flag '%s'" s);
          Hashtbl.add Globals.flags_tbl s true
        end
      ) fl

  (**
   * Parse macro/deck xml alts as alt list
   *
   * @param alts Xml.Element list
   * @return alt list
   *)
  let parse_alts (xmls : Xml.xml list) : alt list =
    List.map (fun alt -> match alt with
      | Xml.Element ("alt", attributes, [Xml.PCData content]) ->
          {content; attributes}
      | Xml.Element ("alt", attributes, []) ->
          {content = ""; attributes}
      | _ ->
          raise (Alt_exception "Illegal alt in macro/deck")
    ) xmls 

  (**
    id : int;
    content : string;
    (** List of nodes this node is connected to *)
    connections : int list;
    * @return graph_node list
   *)
  let parse_nodes (nodes : Xml.xml list) : graph_node list =
    List.map (fun node -> match node with
    | Xml.Element ("node", attributes, children) ->
      (* Get content OR children *)
      let (content, children) : string option * Xml.xml list = begin match children with
        | [Xml.PCData c] -> Some c, []
        | _ -> None, children
      end in
      (* Get connections. Right now just a string, evaluation delayed for eval_content. *)
      let connections : string = begin match find_attribute attributes "connections" with
        | Some (_, value) -> value
        | None -> raise (Graph_exception "Missing connections in graph node")
      end in
      (* Get id *)
      let id : int = begin match find_attribute attributes "id" with
        | Some (_, id) -> int_of_string id
        | None -> raise (Graph_exception "Missing id of graph node")
      end in
      (* Get setFlag *)
      let set_flag : string option = begin match find_attribute attributes "setFlag" with
        | Some (_, expr) -> Some expr
        | None -> None
      end in
      (* graph_node *)
      {
        id;
        content;
        connections;
        set_flag;
        children;
      }
    | _ ->
        raise (Graph_exception "Illegal node in graph")
    ) nodes 

  (**
   * Parse macro tag
   *
   * @param attrs Xml.xml list
   * @param alts Xml.xml list
   * @param state
   * @return (macro * namespace option)
   *)
  let parse_macro attrs alts (state : state) : (macro * namespace option) =
    let namespace_name = find_attribute attrs "namespace" in
    let macro_name = find_attribute attrs "name" in
    begin match macro_name, namespace_name with
    | Some ("name", macro_name), Some ("namespace", namespace_name) ->
        let alts = parse_alts alts in
        let namespace = get_namespace_or_create state namespace_name in
        ({name = macro_name; alts}, Some namespace)
    | Some ("name", macro_name), None ->
        let alts = parse_alts alts in
        ({name = macro_name; alts}, None)
    | None, _ ->
        raise (Macro_exception "Macro has no name attribute")
    | _, _ ->
        raise (Internal_error "Macro definition is super weird")
    end

  (**
   * Store macro in macro hash table. Raise exception if macro with this name
   * already exists.
   *
   * @param macro
   * @param namespace namespace
   * @return unit
   *)
  let store_macro (macro : macro) namespace =
    if Hashtbl.mem namespace.macro_tbl macro.name then
      raise (Macro_exception (sprintf "Macro with name '%s' already exists" macro.name))
    else
      Hashtbl.add namespace.macro_tbl macro.name macro

  (**
   * @param attrs Xml.xml list
   * @param state
   * @return (dice * namespace option)
   *)
  let parse_dice attrs (state : state) : (dice * namespace option) =
    let namespace_name = find_attribute attrs "namespace" in
    let dice_name = find_attribute attrs "name" in
    let sides = find_attribute attrs "sides" in
    begin match dice_name, namespace_name, sides with
    | Some ("name", dice_name), Some ("namespace", namespace_name), Some ("sides", sides) ->
        let namespace = get_namespace_or_create state namespace_name in
        let sides = int_of_string sides in
        ({name = dice_name; sides}, Some namespace)
    | Some ("name", dice_name), None, Some ("sides", sides) ->
        let sides = int_of_string sides in
        ({name = dice_name; sides}, None)
    | None, _, _ ->
        raise (Dice_exception "Dice tag lacks name attribute")
    | _, _, None ->
        raise (Dice_exception "Dice tag lacks sides attribute")
    | _, _, _ ->
        raise (Internal_error "Dice definition is super weird")
    end

  (**
   * @param dice
   * @param namespace namespace
   * @return unit
   *)
  let store_dice (dice : dice) namespace =
    if Hashtbl.mem namespace.dice_tbl dice.name then
      raise (Macro_exception (sprintf "Dice with name '%s' already exists" dice.name))
    else
      Hashtbl.add namespace.dice_tbl dice.name dice


  (**
   * Look for setFlag attribute in attribute list
   * and sets flags.
   * @param attributes ?
   * @return unit
   *)
  let set_flags attributes =
    let setFlag = find_attribute attributes "setFlag" in
    let flags_list = match setFlag with
      | None ->
          log_trace "eval_alt: found no flag in setFlag?"; 
          None
      | Some (_, flags) ->
         log_trace (sprintf "eval_alt: found setFlag %s" flags);
         Some (Str.split (Str.regexp "[ \t]+") flags)
    in
    store_flags flags_list


  (**
   * Eval macro used in alt or inline, like <alt useMacro="name"> or {#name}
   *
   * @param name string
   * @param namespace namespace
   * @return string
   * @raise Macro_exception if name is not a macro
   *)
  let eval_macro name (namespace : namespace) =
    (* Aux function to check for <alt></alt> *)
    let check_for_empty_content c =
      match c with
      | "" ->
          ()
          (*raise (Alt_exception "Empty content for <alt>. Check so no <alt> is defined as <alt></alt>")*)
      | _ ->
          ()
    in

    if Hashtbl.mem namespace.macro_tbl name then begin
      let macro = Hashtbl.find namespace.macro_tbl name in
      (*let alt = choose_alt macro.alts in*)
      let alts = get_possible_alts macro.alts in
      let random_nr = Dice.dice (List.length alts) in
      let alt = match List.nth alts random_nr with
        | alt -> alt
        | exception Not_found -> raise (Macro_exception "No alt?")
      in
      (*
      log_trace (sprintf "eval_macro: alt: %s" alt.content);
      (List.iter (fun (s1, s2) -> log_trace ("eval_macro: alt attributes: " ^ s1)) alt.attributes);
      *)
      check_for_empty_content alt.content;
      set_flags alt.attributes;
      alt.content
    end else
      raise (Macro_exception (sprintf "useMacro: Found no macro '%s' in namespace '%s'" name namespace.name))

  (**
   * Evaluate a record and return its content
   *
   * @param record_name string
   * @param var_name string
   * @param namespace namespace
   * @return string
   * @raise Record_exception
   *)
  let eval_record record_name var_name namespace =
    let record = try Hashtbl.find namespace.record_tbl record_name with
      Not_found -> raise (Record_exception (sprintf "No such record: '%s'" record_name))
    in
    let var = try Hashtbl.find record.values var_name with
      Not_found -> raise (Record_exception (sprintf "No such tag '%s' in record '%s'" var_name record_name))
    in
    var

  (**
   * Evaluate variable and return its content
   *
   * @param var_name string
   * @param namespace namespace
   * @return string
   * @raise Variable_exception
   *)
  let eval_var var_name namespace =
    try
      Hashtbl.find namespace.var_tbl var_name
    with
        Not_found -> raise (Variable_exception (sprintf "Could not find variable '%s' in namespace '%s'. Check that you defined it with <variable name=\"%s\">..." var_name namespace.name var_name))

  (**
   * Roll the dice and return string of value.
   * @param name string
   * @param int number_of_dice
   * @param namespace namespace
   * @return string
   * @raise Dice_exception
   *)
  let eval_dice dice_name number_of_dice namespace =
    let dice = try Hashtbl.find namespace.dice_tbl dice_name with
      Not_found -> raise (Dice_exception (sprintf "No such dice: '%s'" dice_name))
    in
    let value = ref 0 in
    for i = 1 to number_of_dice do
      value := !value + Random.int (dice.sides) + 1;
    done;
    string_of_int !value

  (**
   * Inline input
   *
   * @param string variable_name
   * @param state state
   * @param namespace namespace
   * @return string
   *)
  let eval_input (variable_name : string) (state : state) (namespace : namespace) =
    let buffer = Io.input_line stdin in
    Hashtbl.add namespace.var_tbl variable_name (String.trim buffer);
    ""

  (**
   * @param namespace namespace
   * @param typ string
   * @param name string
   * @return unit
   *)
  let clear namespace attrs = 

    (* Fetch name from attributes *)
    let name = match find_attribute attrs "name" with
      | Some ("name", name) -> name
      | Some _ -> raise (Internal_error "Internal error in clear function 1")
      | None -> raise (Clear_exception "Missing attribute 'name' for <clear> tag")
    in

    (* Check if ignore-not-found is set to 1 *)
    let ignore_not_found = true in (*match find_attribute attrs "ignore-not-found" with
      | Some ("ignore-not-found", "1") -> true
      | Some _ | None -> false
    in *)

    let typ = find_attribute attrs "type" in
    match typ with
    | Some ("type", "macro") -> begin
        try
          let _ = Hashtbl.find namespace.macro_tbl name in
          Hashtbl.remove namespace.macro_tbl name;
        with
          Not_found -> 
            if ignore_not_found then
              ()
            else
              raise (Clear_exception (sprintf "Found no macro to clear with name '%s'" name))
    end
    | Some ("type", "variable") -> begin
        try
          let _ = Hashtbl.find namespace.var_tbl name in
          Hashtbl.remove namespace.var_tbl name;
        with
          Not_found -> 
            if ignore_not_found then
              ()
            else
              raise (Clear_exception (sprintf "Found no variable to clear with name '%s'" name))
    end
    | Some ("type", "flag") -> begin
          if Hashtbl.mem Globals.flags_tbl name then
            Hashtbl.remove Globals.flags_tbl name
          else if not ignore_not_found then
            raise (Clear_exception (sprintf "Found no flag to clear with name '%s'" name))
    end
    | Some ("type", typ) ->
        raise (Clear_exception (sprintf "Unknown type in clear: %s" typ))
    | Some _ ->
        raise (Internal_error "Internal error in clear function 2")
    | None ->
        raise (Clear_exception "Missing attribute 'type' for <clear> tag")

  (**
   * Choose card from deck and remove it from the deck.
   * Return card content.
   *
   * @param string into - Used with tag <pick> when record name does not determin into which record variable card is put.
   * @param deck_name string
   * @param state state
   * @param namespace namespace
   * @return string
   *)
  let rec eval_deck ?(into : string = "") (deck_name : string) (state : state) (namespace : namespace) : string =
    log_trace "eval_deck\n";
    let deck = try Hashtbl.find namespace.deck_tbl deck_name with
      | Not_found -> raise (Deck_exception (sprintf "Found no deck with name '%s'." deck_name))
    in

    (* Abort or shuffle if no cards are left in deck *)
    let deck = begin match List.length deck.cards, deck.shuffle_on_empty with
      | 0, true -> {deck with cards = deck.trash; trash = []}
      | 0, false -> raise (Deck_exception (sprintf "No more cards in deck '%s'." deck_name));
      | _, _ -> deck
    end in
    let filtered_cards : card list = get_possible_cards deck.cards in
    let pick_nr        : int       = Dice.dice (List.length filtered_cards) in
    let picked_card    : card      = List.nth filtered_cards pick_nr in

    (* Filter out picked card *)
    let i         : int ref   = ref (-1) in
    let new_cards : card list = List.filter
      (fun _ ->
        i := !i + 1;
        !i != pick_nr
      )
      deck.cards
    in

    (* New deck with picked card moved to trash *)
    let new_deck : deck = {
      deck with
      cards = new_cards;
      trash = picked_card :: deck.trash
    } in

    (* Store new deck in namespace *)
    Hashtbl.remove namespace.deck_tbl deck.name;
    Hashtbl.add namespace.deck_tbl new_deck.name new_deck;

    match picked_card with
    | exception Not_found -> raise (Deck_exception ("Found no card"))
    | Alt alt ->
        (* Create a new deck without the card we picked *)
        (* Card = alt in this context *)
        (* Filter logic:
         * We want to filter the card we just picked.
         * All cards that are not identical to this card should return false (not filter)
         * The card is identical to the card we picked IF:
         *   content = content AND attributes are equal (name && content of attribues are equal)
         *)
        (** TODO: Start from 0 or 1? Previously used Core, but won't compile on ARM. *)
        log_trace "old_deck: ";
        log_trace "new deck: ";
        print_deck new_deck;
        (* Build Xml.xml <alt> out of record *)
        let alt : Xml.xml = Xml.Element ("alt", alt.attributes, [Xml.PCData alt.content]) in
        let result : string = eval_alt alt state namespace in
        log_trace (sprintf "eval_deck: result of eval_alt = %s\n" result);
        result
    | Record record ->
        (* Push record in card to namespace *)
        if into <> "" then
          Hashtbl.replace namespace.record_tbl into record
        else begin
          let name : string = match record.name with
            | Some s -> s
            | None -> raise (Deck_exception "Found no name for record in deck")
          in
          Hashtbl.replace namespace.record_tbl name record
        end;
        log_trace "inserted record";
        ""

  (**
   * @param graph
   * @return (graph_node, picked_nr)
   *)
  and pick_next_node (graph : graph) (state : state) (namespace : namespace) =
    let current_node : graph_node = match List.find_opt (fun n -> n.id = graph.current_node) graph.nodes with
        | Some node -> node
        | None -> raise (Graph_exception (sprintf "Found no current node with id %d" graph.current_node))
    in
    let connections : string    = eval_content current_node.connections state namespace in
    let regexp : Str.regexp     = Str.regexp "," in
    let splits : string list    = Str.split regexp connections in
    let connections : int list  = List.map (fun number -> int_of_string number) splits in
    let nr_of_connections : int = List.length connections in
    let random_pick : int       = Dice.dice (nr_of_connections) in
    let picked_nr : int         = List.nth connections random_pick in
    let picked_node             = List.find (fun n -> n.id = picked_nr) graph.nodes in
    (picked_node, picked_node.id)

  (**
   * @param graph_name string
   * @param state state
   * @param namespace namespace
   * @return string
   *)
  and eval_graph (graph_name : string) (state : state) (namespace : namespace) : string =
    log_trace "eval_graph";
    let graph = try Hashtbl.find namespace.graph_tbl graph_name with
      | Not_found -> raise (Graph_exception (sprintf "Found no graph with name '%s'" graph_name))
    in

    (* Remove old graph and store new with new current node *)
    let (next_node, next_node_id) : graph_node * int = pick_next_node graph state namespace in
    let new_graph : graph = {graph with current_node = next_node_id} in
    Hashtbl.remove namespace.graph_tbl graph.name;
    Hashtbl.add namespace.graph_tbl new_graph.name new_graph;
    (* Set flag if any *)
    begin match next_node.set_flag with
      | Some flag_name -> store_flags (Some [flag_name])
      | None -> ()
    end;
    (* Eval content in node *)
    begin match next_node.content, next_node.children with
      | Some s, [] -> eval_content s state namespace
      | None, [] -> raise (Graph_exception "Invalid graph node: Empty node content, and no XML children")
      | None, children ->
          List.iter (fun c -> ignore (print_tag c state namespace)) children;
          ""
      | _, _ -> raise (Graph_exception ("Unsupported combination of string and XML children in graph node"))
    end

  (**
   * @param content string
   * @param pattern string Regexp pattern as string to match namespace for variable, record, macro etc
   * @param state state
   * @param default_namespace namespace
   * @return (namespace, content)
   *)
  and get_inline_namespace
    (content :string)
    (pattern : string)
    (state : state)
    (default_namespace : namespace) =
    (* Check for inline namespace like {namespace\var} *)
    let matches = try Pcre.exec_all ~pat:pattern content with Not_found -> [||] in
    let matches = Array.to_list (
      Array.map (fun m ->
        let substrings = Pcre.get_substrings m in
        let s = substrings.(0) in
        (* Strip {} *)
        let s = String.sub s 1 (String.length s - 2) in
        let slash_index = String.index s '\\' in
        let s = String.sub s 0 slash_index in
        (*printf "substring = %s\n" s;*)
        s
      ) matches
    ) in

    (* Abort if we found more than one namespace *)
    if List.length matches > 1 then
      raise (Internal_error (sprintf "get_inline_namespace: More than one namespace found for content '%s'" content));

    if List.length matches = 0 then
      (default_namespace, content)

    (* Found inline namespace, so pick first and use that instead, and remove namespace from content *)
    else begin match List.hd matches with
    | exception Not_found->
        raise (Internal_error (sprintf "get_inline_namespace: Found one namespace, but no match?"))
    | namespace_name ->
      begin match get_namespace state namespace_name with
      | None ->
          raise (Namespace_exception (sprintf "Found no namespace with name '%s' for content '%s'" namespace_name content))
      | Some namespace ->
          let content = Pcre.replace ~pat:(namespace_name ^ "\\\\") ~templ:"" content in
          namespace, content
      end
    end

  (**
   * Replace inline variable
   * {var}
   * {namespace\var}
   *
   * @param content string Content from <sentence>
   * @param var_tbl hash table
   * @return string
   *)
  and replace_inline_variable content state (namespace : namespace) =
    (* Check for inline namespace like {namespace\var} *)
    let pattern = "{[a-zA-Z0-9_]+\\\\(?:[a-zA-Z0-9_]+)}" in
    let (namespace, content) = get_inline_namespace content pattern state namespace in

    let matches = try Pcre.exec_all ~pat:"{[a-zA-Z0-9_]+}" content with Not_found -> [||] in
    let matches = Array.to_list (
      Array.map (fun m ->
        let substrings = Pcre.get_substrings m in
        let s = substrings.(0) in
        let s = String.sub s 1 (String.length s - 2) in
        s
      ) matches
    ) in
    let matches_uniq = List.sort_uniq (fun a b -> 1) matches in

    (* Replace inline variables, like {this} *)
    let rec replace_variables matches content = match matches with
      | [] -> content
      | var_name::vars ->
          let pattern = sprintf "{%s}" var_name in
          let var_content = eval_var var_name namespace in
          let new_content = Pcre.replace ~pat:pattern ~templ:var_content content in
          replace_variables vars new_content
    in
    replace_variables matches_uniq content

  (**
   * Replace inline records like {this.that}
   *
   * @param content string
   * @param state state
   * @param namespace
   * @return string
   *)
  and replace_inline_records content state namespace =

    (* Check for inline namespace like {namespace\var.this} *)
    let pattern = "{[a-zA-Z0-9_]+\\\\(?:[a-zA-Z0-9_]+\\.[a-zA-Z0-9_]+)}" in
    let (namespace, content) = get_inline_namespace content pattern state namespace in

    (* Replace records like {this.here} *)
    let matches = try Pcre.exec_all ~pat:"{[a-zA-Z0-9_]+\\.[a-zA-Z0-9_]+}" content with Not_found -> [||] in
    (* Matches as [[key, val], [key, val], ...] *)
    let matches = Array.to_list (
      Array.map(fun m ->
        let substrings = Pcre.get_substrings m in
        let s = substrings.(0) in
        let s = String.sub s 1 (String.length s - 2) in
        Pcre.split ~pat:"\\." s
      ) matches
    ) in
    let rec replace_records matches content = match matches with
      | [] -> content
      | [record_name; var_name] :: xs ->
          (* Get record and var name *)
          let record_content = eval_record record_name var_name namespace in
          let pattern = sprintf "{%s.%s}" record_name var_name in
          let new_content = Pcre.replace ~pat:pattern ~templ:record_content content in
          replace_records xs new_content
      | _ ->
          raise (Internal_error "eval_content: replace_records: Wrong structure in list")
    in
    replace_records matches content

  (**
   * Replace inline macros like {#this}
   *
   * @param content string
   * @param namespace
   * @param namespace
   * @return string
   *)
  and replace_inline_macros content state namespace =
    (* Check for inline namespace like {namespace\#this} *)
    let pattern = "{[a-zA-Z0-9_]+\\\\(?:#[a-zA-Z0-9_]+)}" in
    let (namespace, content) = get_inline_namespace content pattern state namespace in
    (* Replace inline macros, like {#this} *)
    let matches = try Pcre.exec_all ~pat:"{#[a-zA-Z0-9_]+}" content with Not_found -> [||] in
    let matches = Array.to_list (
      ArrayLabels.map matches ~f:(fun m ->
        let substrings = Pcre.get_substrings m in
        let s = substrings.(0) in
        let s = String.sub s 2 (String.length s - 3) in
        s
      )
    ) in
    (**
     * @param matches
     * @param content
     * @return string?
     *)
    let rec replace_macros matches content = 
      match matches with
      | [] -> content
      | mat::tail ->
          let macro_content = eval_macro mat namespace in
          let pattern = sprintf "{#%s}" mat in
          let content = Pcre.replace_first ~pat:pattern ~templ:macro_content content in
          replace_macros tail content
    in
    replace_macros matches content

  (**
   * Replaces inline content like {"this"}. Used for inline randomization, like
   * in {#macro|"content"|variable}
   *
   * @param con string Content
   * @return string
   *)
  and replace_inline_content con =
    let matches = try Pcre.exec_all ~pat:"{\"[a-za-z0-9_\\s]+\"}" con with not_found -> [||] in
    let matches = Array.to_list (
      ArrayLabels.map matches ~f:(fun m ->
        let substrings = Pcre.get_substrings m in
        let s = substrings.(0) in
        let s = String.sub s 1 (String.length s - 2) in
        s
      )
    ) in
    let rec replace_content matches con = match matches with
      | [] -> con
      | mat::tail ->
          (*
          log_trace "mat = %s\n" mat;
          log_trace "con = %s\n" con;
          *)
          let replacement = String.sub mat 1 (String.length mat - 2) in
          let mat = sprintf "{%s}" mat in
          let mat = Pcre.quote mat in
          let con = Pcre.replace ~pat:mat ~templ:replacement con in
          replace_content tail con
    in
    replace_content matches con

  (**
   * Replace inline deck like {$deck}
   *
   * @param con string Content
   * @param deck_tbl hash table
   * @return string
   *)
  and replace_inline_deck con state namespace =
    let matches = try Pcre.exec_all ~pat:"{\\$[a-zA-Z0-9_]+}" con with Not_found -> [||] in
    let matches = Array.to_list (
      ArrayLabels.map matches ~f:(fun m ->
        let substrings = Pcre.get_substrings m in
        let s = substrings.(0) in
        let s = String.sub s 2 (String.length s - 3) in
        s
      )
    ) in
    let rec replace_content matches con = match matches with
    | [] -> con
    | mat::tail ->
        let card_content = eval_deck mat state namespace in
        let pattern = sprintf "{\\$%s}" mat in
        let con = Pcre.replace_first ~pat:pattern ~templ:card_content con in
        replace_content tail con
    in
    replace_content matches con

  (**
   * Eval content to replace inline variables, records and macros
   * Used in sentence and alt
   *
   * @param con string content
   * @param state state
   * @param namespace namespace
   * @return string
   *)
  and eval_content (con : string) (state : state) (namespace : namespace) =
    log_trace "eval_content";
    let matches = try Pcre.exec_all ~pat:"{[\"$#%@:\\+\\-\\.\\\\\\|\\(\\)!?èÈòÒùÙàÀìÌỳỲéÉóÓúÚíÍáÁýÝẼẽõÕÃãŨũĩĨêâîûÊÂÛÎëËüÜïöåäöÅÄÖa-zA-Z0-9_\\s]+}" con with not_found -> [||] in
    let matches_and_replacements = Array.map (fun m ->
        let substrings = Pcre.get_substrings m in
        let s_with_braces = substrings.(0) in
        let s_without_braces = String.sub s_with_braces 1 (String.length s_with_braces - 2) in
        let ast = parse_ast s_without_braces in
        let evaled = eval_ast ast state namespace in
        (s_with_braces, evaled)
      ) matches
    in
    let new_con = ref con in
    Array.iter (fun (match_with_braces, replacement) ->
      let match_with_brackets_quote = Pcre.quote match_with_braces in
      new_con := Pcre.replace_first ~pat:match_with_brackets_quote ~templ:replacement !new_con;
      (* Check if eval was evaled to another pattern. *)
      let matches = try Pcre.exec_all ~pat:"{[\"$#%@:\\+\\-\\.\\\\\\|\\(\\)!?èÈòÒùÙàÀìÌỳỲéÉóÓúÚíÍáÁýÝẼẽõÕÃãŨũĩĨêâîûÊÂÛÎëËüÜïöåäöÅÄÖa-zA-Z0-9_\\s]+}" replacement with not_found -> [||] in
      if Array.length matches > 0 then begin
        let final_replacement = eval_content replacement state namespace in
        let replacement_quote = Pcre.quote replacement in
        new_con := Pcre.replace_first ~pat:replacement_quote ~templ:final_replacement !new_con;
      end
    ) matches_and_replacements;
    log_trace (sprintf "new_con = %s" !new_con);
    !new_con

  (**
   * Eval <alt> to its content
   *
   * @param alt Xml.Element
   * @param state state
   * @return string
   *)
  and eval_alt (alt : Xml.xml) (state : state) (namespace : namespace) =
    log_trace (Xml.to_string alt);
    let attributes = Xml.attribs alt in

    (* Find attribute setFlag *)
    set_flags attributes;

    (* Get macro to use as (string * string) tuple *)
    (** TODO: Add useDice *)
    let useMacro = find_attribute attributes "useMacro" in
    let useDeck = find_attribute attributes "useDeck" in
    let _include = find_attribute attributes "include" in

    (* Get content either from macro, deck or alt.content *)
    let cont = 
      match useMacro, useDeck, _include with
      | None, None, None ->
          fetch_content alt
      | Some (_, macro_name), None, None ->
          eval_macro macro_name namespace
      | None, Some (_, deck_name), None ->
          log_trace "eval_alt: useDeck";
          eval_deck deck_name state namespace
      | None, None, Some (_, filename) ->
          log_trace "eval_alt: file_to_string";
          (try file_to_string filename state with
          | ex ->
              raise (Include_exception ("Can't evalutate <alt>", ex)))
      | _ ->
          raise (Alt_exception "Both useMacro, useDeck and include attributes?")
    in

    eval_content cont state namespace

  (**
   * Eval sentence, changin {bla} to variable content
   * @param sen string
   * @param state
   * @param namespace
   * @return string
   *)
  and eval_sen sen state namespace =
    try (
      eval_content sen state namespace
    )
    with
      Not_found -> sen

  (**
   * Print a sentence with random alt.
   *
   * @param sentence Xml.xml
   * @return string
   *)
  and print_sentence sentence (state : state) namespace =
    log_trace "print_sentence";
    let sen = String.trim (fetch_content (sentence)) in
    try (
      let sen = eval_sen sen state namespace in
      let alt = choose_alt_from_sentence (fetch_nodes (sentence) "alt") in
      match alt, sen with
        | None, _ -> sen
        | Some alt, "" ->
            (eval_alt alt state namespace)
        | Some alt, sen ->
            sen ^ " " ^ (eval_alt alt state namespace)
            (* TODO: If alt is ""? *)
    )
    with
      ex ->
        log_trace "exception in print_sentence";
        raise (Sentence_problem (sen, string_of_exn ex))

  (**
   * Check so that only Xml.Element "alt" are in the list
   *
   * @param alts Xml.Element list
   * @return bool
   *)
  and only_alts alts =
    List.for_all (fun a -> match a with
      | Xml.Element ("alt", _, _) -> true
      | _ -> false
    ) alts

  (**
   * Return true if variable name is free (unused), otherwise false
   *
   * @param name string
   * @return bool
   *)
  and variable_name_free name var_tbl =
    not (Hashtbl.mem var_tbl name)

  (**
   * Check if all <alt>:s have a content (except macro and deck alts)
   *
   * @param alts Xml.Element list
   * @return bool
   *)
  and all_alts_have_content alts =
    List.for_all (function
      | Xml.Element ("alt", _, [Xml.PCData content]) ->
          true
      | Xml.Element ("alt", attrs, _) when (find_attribute attrs "useMacro" != None) ->
          true
      | Xml.Element ("alt", attrs, _) when (find_attribute attrs "useDeck" != None) ->
          true
      | Xml.Element ("alt", _, _) ->
          false
      | _ ->
          false
    ) alts


  (**
   * Check if we can add new variable with alts and name
   * Raise exception if not
   *
   * @param name string Name of new variable
   * @param alts Xml.Element list
   * @return unit
   *)
  and variable_is_ok name alts namespace =
    if not (only_alts alts) then
      raise (Variable_exception (sprintf "Only <alt> allowed in variable tag for variable '%s\\%s'" namespace.name name))
    else if not (all_alts_have_content alts) then
      raise (Variable_exception (sprintf "Some <alt> in variable '%s\\%s' does not have any content" namespace.name name))
    else if not (variable_name_free name namespace.var_tbl) then
      raise (Variable_exception (sprintf "<variable> with name '%s\\%s' is already in use, can only be defined once." namespace.name name))
    else
      ()

  (**
   * Return true if record definition is valid, both for records with and without <alt> children.
   *
   * @param string name
   * @param Xml.Element list children
   * @param record_tbl record_tbl
   * @return bool
   *)
  and record_is_ok (name : string) (children : Xml.xml list) (record_tbl : record_tbl) : bool =
    (* Check so record name is free *)
    if Hashtbl.mem record_tbl name then begin
      log_trace "record_is_ok";
      log_trace (Printexc.get_backtrace ());
      raise (Record_exception (sprintf "Record name '%s' is already in use" name))
    end;

    (* Check so children are not empty *)
    if List.length children = 0 then begin
      raise (Record_exception (sprintf "Record '%s' has no <alt>" name))
    end;

    (* Assume that if first child is <alt>, it's an <alt>-record *)
    let use_alt : bool = match List.hd children with
      | Xml.Element ("alt", _, _) -> true
      | _ -> false
    in

    (* Each alt must have equal number of children with same structure *)
    let compare children = match children with
      | [] -> true
      | x::xs -> List.for_all (fun alt -> match x, alt with
          | Xml.Element ("alt", _, subtags), Xml.Element ("alt", _, subtags2) -> begin
              try
                (* Check so each var has same tag name *)
                List.for_all2 (fun var1 var2 -> match var1, var2 with
                  | Xml.Element (name1, _, [Xml.PCData _]), Xml.Element (name2, _, [Xml.PCData _]) ->
                      name1 = name2
                  | _ -> false
                ) subtags subtags2
              with
                Invalid_argument _ ->
                  raise (Record_exception (sprintf "Not equal number of <alt> in <record> '%s'" name))
              end
          | _, _ -> false
      ) xs
    in

    (* Raise exception if alts are not equal *)
    if use_alt && not (compare children) then begin
      raise (Record_exception (sprintf "Wrong structure in <record> '%s')" name))
    end;

    true

  (**
   * Check so deck declaration is OK. Name is not used.
   *
   * @param name string
   * @param alts Xml.Element list
   * @return bool
   *)
  and deck_is_ok name alts deck_tbl =
    (* Check so deck name is free *)
    if Hashtbl.mem deck_tbl name then begin
      raise (Deck_exception (sprintf "Deck name '%s' is already in use" name))
    end;
    true

  (**
   * @param name string
   * @param nodes graph_node list
   * @return bool
   * @throws Graph_exception
   *)
  and graph_is_ok name nodes graph_tbl =
    if Hashtbl.mem graph_tbl name then begin
      raise (Graph_exception (sprintf "Graph name '%s' is already in use" name))
    end;
    true

  (**
   * Eval <variable>
   * Adds variable to variable hash table
   *
   * @param name string
   * @param alts Xml.Element list
   * @param state state
   * @return void
   *)
  and save_variable name alts (state : state) namespace =
    match choose_alt alts with
    | None ->
        raise (Variable_exception (sprintf "No alt could be chosen for variable '%s'" name))
    | Some alt ->
      let content = eval_alt alt state namespace in
      Hashtbl.add namespace.var_tbl name content

  (**
   * Parse xml list into cards.
   *)
   and parse_cards (xmls : Xml.xml list) (state : state) (namespace : namespace) : card list =
    List.map
      (fun (xml : Xml.xml) : card -> match xml with
        | Xml.Element ("alt", attributes, [Xml.PCData content]) ->
            Alt {content; attributes}
        | Xml.Element ("record", [("name", name)], fields)
        | Xml.Element ("card", [("name", name)], fields) -> 
            Record (parse_record (Some name) fields state namespace)
        | Xml.Element ("card", [], fields) -> 
            Record (parse_record None fields state namespace)
        | Xml.Element (tag, _, _) ->
            raise (Deck_exception ("Cannot parse card with tag " ^ tag))
        | Xml.PCData text ->
            raise (Deck_exception ("Deck can't contain only text: " ^ text))
      )
      xmls

  (**
   * Parse name and XML and return a record
   *
   * @param string option name
   * @param Xml.xml list children - Record fields
   * @return record
   *)
  and parse_record (name : string option) (children : Xml.xml list) (state : state) (namespace : namespace) : record =
    (* Assume that if first child is <alt>, it's an <alt>-record *)
    let use_alt : bool = match List.hd children with
      | Xml.Element ("alt", _, _) -> true
      | _ -> false
    in
    (* Choose alt randomly OR, if no <alt> are used, pick all children. *)
    let alt : Xml.xml = if use_alt then
      match List.nth children (Dice.dice (List.length children)) with
        | alt -> alt
        | exception Not_found -> raise (Record_exception "No alt?")
    else
      Xml.Element ("alt", [], children)
    in

    (* Store flags if present *)
    let flags_list : string list option = get_flags alt in
    store_flags flags_list;

    (* Store vars from alt in record hash table *)
    let record = {
      name;
      values = Hashtbl.create 20;
    }
    in
    List.iter (fun var ->
      match var with
      | Xml.Element (name, [], [Xml.PCData content]) ->
          let content : string = eval_content content state namespace in
          Hashtbl.add record.values name content
      | Xml.Element (var_name, _, []) ->
          raise (Record_exception (sprintf "No empty content allowed in '%s' for record" var_name))
      | Xml.Element (var_name, _, _) ->
          raise (Record_exception (sprintf "No attributes allowed in '%s' for record" var_name))
      | _ ->
          raise (Record_exception (sprintf "Unknown error in record"))
    ) (Xml.children alt);
    record

  (**
   * Adds record to record hash table
   *
   * @param name string
   * @param alts Xml.Element list
   * @param record_tbl record_tbl
   * @return unit
   *)
  and store_record (name : string) (alts : Xml.xml list) (record_tbl : record_tbl) (state : state) (namespace : namespace) : unit =
    let record : record = parse_record (Some name) alts state namespace in

    (* Add record to records hash table *)
    Hashtbl.add record_tbl name record

  (**
   * Store deck in deck_tbl
   *
   * @param deck deck
   * @param deck_tbl deck_tbl
   * @return unit
   *)
  and store_deck (deck : deck) deck_tbl =
    if Hashtbl.mem deck_tbl deck.name then
      raise (Deck_exception (sprintf "Deck with name '%s' already exists" deck.name))
    else
      Hashtbl.add deck_tbl deck.name deck

  (**
   * Store graph in graph_tbl
   *
   * @param graph
   * @param graph_tbl
   * @return unit
   *)
  and store_graph (graph : graph) graph_tbl =
    if Hashtbl.mem graph_tbl graph.name then
      raise (Graph_exception (sprintf "Graph with name '%s' already exists" graph.name))
    else
      Hashtbl.add graph_tbl graph.name graph

  (**
   * Parse children in <list> tag to list items.
   *
   * @param string list_type
   * @param Xml.Element list children
   * @return list_item list
   *)
  and parse_list_items (list_type : string) (children : Xml.xml list) (state : state) (namespace : namespace) : list_item list =
    match list_type with
      | "record" ->
          List.map (fun xml -> match xml with
            | Xml.Element ("record", [("name", name)], alts) -> Record (parse_record (Some name) alts state namespace)
            | _ -> raise (Tag_exception "Unsupported record list element")
          ) children
      | s -> raise (Tag_exception (sprintf "Unknown list type: %s" s))

  (**
   * Store list in list table.
   *
   * @param string name List name
   * @param list_ list_
   * @param list_
   *)
  and store_list (name : string) (list_ : list_) (namespace : namespace) : unit =
    log_trace "store_list";
    if Hashtbl.mem namespace.list_tbl name then
      raise (Tag_exception (sprintf "List with name '%s' already exists" name))
    else begin
      (* Store all list items. *)
      List.iter (fun item -> match item with
        | Record record ->
            let name : string = match record.name with
              | Some s -> s
              | None -> raise (Deck_exception "Could not store list item, found no name for record")
            in
            Hashtbl.add namespace.record_tbl name record
        | _ -> raise (Tag_exception "Unsupprted item type in store_list")
      ) list_.items;
      Hashtbl.add namespace.list_tbl name list_
    end

  (**
   * Run an if statement if content op value
   *
   * @param op Operation to apply (=, >, <)
   * @param content Left-hand value
   * @param value Right-hand value
   * @param children
   * @param state
   * @param namespace
   * @return string
   *)
  (*and run_if_operation (op : 'a -> 'a -> bool) (content : 'a) (value : 'a) (children : Xml.xml list) (state : state) (namespace : namespace) : string =*)
  and run_if_operation: 'a. ('a -> 'a -> bool) -> 'a -> 'a -> Xml.xml list -> state -> namespace -> string = fun op content value children state namespace ->
    log_trace "run_if_operation";
    begin match children with
      | [
          Xml.Element ("then", [], then_children);
          Xml.Element ("else", [], else_children)
        ] ->
            if op content value then
              print_sentences (Xml.Element ("", [], then_children)) state namespace
            else
              print_sentences (Xml.Element ("", [], else_children)) state namespace
      | children ->
            if op content value then
              print_sentences (Xml.Element ("", [], children)) state namespace
            else
              ""
    end

  (**
   * @param xml_element
   * @param state state
   * @param namespace namespace
   * @return string
   *)
  and print_tag (xml_element : Xml.xml) (state : state) (namespace : namespace) : string =
      let result : string = try begin match xml_element with

        (* <sentence attr="...">...</sentence> *)
        (* Never create namespace when using <sentence>, only get *)
        | Xml.Element ("sentence", attrs, _)
        | Xml.Element ("print", attrs, _) when (flags_is_ok attrs) ->
            let newline : string = if config_is_on state "newline" then "\n" else "" in
            let namespace_name = find_attribute attrs "namespace" in
            begin match namespace_name with
            | None ->
              print_sentence xml_element state namespace ^ " " ^ newline
            | Some ("namespace", namespace_name) ->
                let local_namespace = get_namespace state namespace_name in
                begin match local_namespace with
                | None ->
                    let local_namespace = create_namespace state namespace_name in
                    print_sentence xml_element state local_namespace ^ " " ^ newline
                | Some local_namespace ->
                    print_sentence xml_element state local_namespace ^ " " ^ newline
                end
            | Some (attr, _) ->
                let sen = String.trim (fetch_content xml_element) in
                raise (Sentence_problem (sen, sprintf "Unknown attritube: %s" attr))
            end
        | Xml.Element ("sentence", [], _)
        | Xml.Element ("print", [], _) ->
            let newline : string = if config_is_on state "newline" then "\n" else "" in
            print_sentence xml_element state namespace ^ " " ^ newline

        (* Empty sentence *)
        (* TODO: Why would anyone do this? *)
        | Xml.Element ("sentence", _, _)
        | Xml.Element ("print", _, _) ->
            ""

        (* <br /> *)
        | Xml.Element ("br", _, _) ->
            "\n"

        (* <loop times="2"> *)
        | Xml.Element ("loop", [("times", times)], children) ->
            let str = ref "" in
            let fold_aux a b =
                a ^ (print_tag b state namespace)
            in
            for i = 1 to int_of_string times do
                str := !str ^ (List.fold_left fold_aux "" children)
            done;
            (* TODO: Already output in print_tag above, but won't work with CGI *)
            (*!str*)
            ""

        (* <loop rand="10"> *)
        | Xml.Element ("loop", [("rand", rand)], children) ->
            let str = ref "" in
            let fold_aux a b =
                a ^ (print_tag b state namespace)
            in
            let random_nr = Dice.dice (int_of_string rand) + 1 in
            for i = 1 to random_nr do
                str := !str ^ (List.fold_left fold_aux "" children)
            done;
            (* TODO: Already output in print_tag above, but won't work with CGI *)
            (*!str*)
            ""

        (* <loop until="flagisset"> ... </loop> *)
        | Xml.Element ("loop", [("until", condition)], children) ->
            let str : string ref = ref "" in
            (**
             * @param string a
             * @param string b
             * @return string
             * @raise End_loop
             *)
            let fold_aux (a : string) (b : Xml.xml) =
                if eval_flag_expr condition then
                  raise End_loop
                else
                  a ^ (print_tag b state namespace);
            in
            begin
              try 
                for i = 1 to 9999 do
                    str := !str ^ (List.fold_left fold_aux "" children)
                done;
              with
                | End_loop -> ()
            end;
            (* TODO: Already output in print_tag above, but won't work with CGI *)
            (*!str*)
            ""

        | Xml.Element ("loop", [("list", list_name); ("record", variable)], children) ->
            let list_ = try Hashtbl.find namespace.list_tbl list_name with
              | Not_found -> raise (Tag_exception (sprintf "Could not find list with name %s" list_name))
            in
            (* Loop through all items with all XMl children of the loop. *)
            List.iter (fun list_item ->
              List.iter (fun child ->
                begin match list_item with
                  | Record record -> Hashtbl.replace namespace.record_tbl variable record
                  | _ -> raise (Tag_exception "Unsupported list item type in print_tags (loop)")
                end;
                (* TODO: Don't return string, but use a factored out output buffer instead *)
                ignore (print_tag child state namespace);
              ) children;
            ) list_.items;
            ""

        (* <loop> with faulty attributes *)
        | Xml.Element ("loop", _, _) ->
            raise (Loop_exception "Faulty loop construct - no 'times' or 'rand' attribute found")

        | Xml.Element ("list", [("name", list_name)], children) ->
            let list_type : string = match List.hd children with
              | Xml.Element ("record", _, _) -> "record"
              | _ -> raise (Tag_exception "Unsupported list type")
            in
            let items : list_item list = parse_list_items list_type children state namespace in
            let list_ : list_ = {
              name = list_name;
              list_type;
              items;
            } in
            (* NB: Have to pass entire namespace, since store_list stores all its items. *)
            store_list list_name list_ namespace;
            ""

        | Xml.Element ("list", _, _) ->
            raise (Tag_exception "Invalid list definition")

        (* <setFlag name="flagname" /> *)
        | Xml.Element ("setFlag", [("name", flagnames)], _) ->
            let flags_list = Some (Str.split (Str.regexp "[ \t]+") flagnames) in
            store_flags flags_list;
            ""

        (* <ifSet name="flag AND flag2 ..."> ... </ifSet> *)
        | Xml.Element ("ifSet", [("name", flag_expression)], children) ->
            if flags_is_ok [("ifSet", flag_expression)] then
              print_sentences (Xml.Element ("", [], children)) state namespace
            else
              ""

        (* <macro> *)
        | Xml.Element ("macro", attrs, alts) ->
            (* store macro *)
            let (macro, namespace_option) = parse_macro attrs alts state in
            begin match namespace_option with
            | Some namespace ->
                store_macro macro namespace;
            | None ->
                store_macro macro namespace;
            end;
            "" (* Return empty string *)

        (* TODO: namespace *)
        | Xml.Element ("variable", [("name", name); ("value", value)], []) ->
            if not (variable_name_free name namespace.var_tbl) then
              raise (Variable_exception (sprintf "Variable name already in use: %s" name))
            else begin
              (* TODO: eval_content value *)
              Hashtbl.add namespace.var_tbl name value;
              ""
            end

        (* <variable> *)
        | Xml.Element ("variable", attrs, alts) ->
            (*when (variable_is_ok name alts namespace.var_tbl)->*)
            let name = find_attribute attrs "name" in
            let namespace_name = find_attribute attrs "namespace" in
            begin
              (* Check which attributes we have *)
              match name, namespace_name with
              | Some ("name", name), None ->
                  variable_is_ok name alts namespace;
                  save_variable name alts state namespace
              | Some ("name", name), Some ("namespace", namespace_name) ->
                  (* Don't use default namespace *)
                  let local_namespace = get_namespace_or_create state namespace_name in
                  variable_is_ok name alts local_namespace;
                  save_variable name alts state local_namespace
              | _, _ ->
                  raise (Variable_exception (sprintf "Attributes wrong for variable"))
            end;
            ""

        (* <record> *)
        | Xml.Element ("record", attrs, children)
        | Xml.Element ("card", attrs, children) ->
            let record_name = find_attribute attrs "name" in
            let namespace_name = find_attribute attrs "namespace" in
            begin match record_name, namespace_name with
            | Some ("name", record_name), None ->
                if record_is_ok record_name children namespace.record_tbl then begin
                  store_record record_name children namespace.record_tbl state namespace;
                  ""
                end else
                  (* record_is_ok will throw exception if record is fail *)
                  ""
            | Some ("name", record_name), Some ("namespace", namespace_name) ->
                let namespace = get_namespace_or_create state namespace_name in
                if record_is_ok record_name children namespace.record_tbl then begin
                  store_record record_name children namespace.record_tbl state namespace;
                  ""
                end else
                  (* record_is_ok will throw exception if record is fail *)
                  ""
            | _, _ ->
                raise (Record_exception (sprintf "Attributes wrong for record"))
            end

        (* <deck> *)
        | Xml.Element ("deck", [("name", name)], alts) when (deck_is_ok name alts namespace.deck_tbl) ->
            log_trace "print_sentences: store deck";
            let cards : card list = parse_cards alts state namespace in
            store_deck {name; cards; shuffle_on_empty = false; trash = [];} namespace.deck_tbl;
            ""

        (* <deck name="name" shuffle="true"> *)
        | Xml.Element ("deck", [("name", name); ("shuffle", _)], alts) when (deck_is_ok name alts namespace.deck_tbl) ->
            log_trace "print_sentences: store deck";
            let cards : card list = parse_cards alts state namespace in
            store_deck {name; cards; shuffle_on_empty = true; trash = [];} namespace.deck_tbl;
            ""

        | Xml.Element ("deck", _, _) ->
            raise (Deck_exception "Invalid deck definition")

        | Xml.Element ("pick", [("from", deck_name); ("into", record_name)], _) ->
            ignore(eval_deck ~into:record_name deck_name state namespace);
            ""

        | Xml.Element ("graph", [("name", name)], nodes) when (graph_is_ok name nodes namespace.graph_tbl) ->
            let nodes : graph_node list = parse_nodes nodes in
            let current_node : int = 1 in
            let start_node : int option = None in
            store_graph {name; nodes; current_node; start_node} namespace.graph_tbl;
            ""

        | Xml.Element ("graph", [("name", name); ("start", start)], nodes) when (graph_is_ok name nodes namespace.graph_tbl) ->
            let nodes : graph_node list = parse_nodes nodes in
            let current_node : int      = int_of_string start in
            let start_node : int option = Some current_node in
            store_graph {name; nodes; current_node; start_node} namespace.graph_tbl;
            ""

        | Xml.Element ("graph", _, _) ->
            raise (Graph_exception "Not a valid graph declaration")

        (* Reset graph to start node *)
        | Xml.Element ("reset", [("type", "graph"); ("name", graph_name)], []) ->
            let graph : graph = try Hashtbl.find namespace.graph_tbl graph_name with
              | Not_found -> raise (Graph_exception (sprintf "<reset>: Found no graph with name '%s'" graph_name))
            in
            let reset_to : int = match graph.start_node with
              | None -> 1
              | Some i -> i
            in
            let new_graph : graph = {graph with current_node = reset_to} in
            Hashtbl.replace namespace.graph_tbl new_graph.name new_graph;
            ""

        (* <dice> *)
        | Xml.Element ("dice", attrs, []) ->
            let (dice, namespace_option) = parse_dice attrs state in
            begin match namespace_option with
            | Some namespace ->
                store_dice dice namespace;
            | None ->
                store_dice dice namespace;
            end;
            ""

        (* <include file=""> *)
        | Xml.Element ("include", [("file", filename)], []) ->
            (* TODO: Copied from main.ml, factorize? *)
            (try file_to_string filename state with
            | ex ->
                raise (Include_exception ("Can't include file", ex))
            )

        | Xml.Element ("clear", attrs, []) ->
            clear namespace attrs;
            ""

        (* <sleep time="10"/> *)
        | Xml.Element ("sleep", [("time", time)], []) ->
            Unix.sleep (int_of_string time);
            ""

        (* <sleep/> *)
        | Xml.Element ("sleep", _, []) ->
            Unix.sleep 1;
            ""

        (* <input name="variablename" label="some question"/> *)
        | Xml.Element ("input", [("name", name);("label", label)], []) ->
            let label : string = eval_content label state namespace in
            Io.print_string label;
            Io.flush ();
            let buffer = Io.input_line stdin in
            Hashtbl.add namespace.var_tbl name (String.escaped (String.trim buffer));
            ""

        (* <input name="path" label="Choose your path: " regexp="[1-4]"/> *)
        | Xml.Element ("input", [("name", name);("label", label); ("validation", validation)], []) ->
            let regexp : Str.regexp = Str.regexp validation in
            let label  : string = eval_content label state namespace in
            Io.print_string label;
            Io.flush ();
            let matches : bool ref  = ref false in
            while not !matches do
              let buffer : string     = Io.input_line stdin in
              matches := Str.string_match regexp buffer 0;
              if !matches then
                Hashtbl.add namespace.var_tbl name (String.escaped (String.trim buffer))
              else begin
                Io.print_string label;
                Io.flush ();
              end
            done;
            ""

        | Xml.Element ("input", _, _) ->
            raise (Tag_exception "Invalid input definition")

        | Xml.Element ("if", [("content", content);("equals", value)], children) ->
            (* Strip {} from string *)
            let regexp  : Str.regexp = Str.regexp "\\{\\}" in
            let content : string = Str.global_replace regexp "" content in
            let content : string = eval_content content state namespace in

            run_if_operation (=) content value children state namespace;

        | Xml.Element ("if", [("content", content);(operation, value)], children) ->
            let regexp  : Str.regexp = Str.regexp "\\{\\}" in
            let content : string = Str.global_replace regexp "" content in
            let content : string = eval_content content state namespace in

            let variable : int option = int_of_string_opt content in
            let value    : int option = int_of_string_opt value in

            let operation = match operation with
              | "higherThan" -> (>)
              | "lessThan"   -> (<)
              | _ -> raise (Tag_exception ("Invalid operation in <if> tag: " ^ operation))
            in

            run_if_operation operation variable value children state namespace;

        (* <if variable="variablename" equals="value"> ... </if> *)
        | Xml.Element ("if", [("variable", variable_name);("equals", value)], children) ->
            let variable = try Hashtbl.find namespace.var_tbl variable_name with
              | Not_found -> raise (Tag_exception "variable not found in <if> tag")
            in

            run_if_operation (=) variable value children state namespace;

        | Xml.Element ("if", [("variable", variable_name);(operation, value)], children) ->
            let variable : string = try Hashtbl.find namespace.var_tbl variable_name with
              | Not_found -> raise (Tag_exception "variable not found in <if> tag")
            in

            let variable : int option = int_of_string_opt variable in
            let value    : int option = int_of_string_opt value in

            let operation = match operation with
              | "higherThan" -> (>)
              | "lessThan"   -> (<)
              | _ -> raise (Tag_exception ("Invalid operation in <if> tag: " ^ operation))
            in

            (* Abort if we can't parse integer *)
            begin match variable, value with
              | None, _ | _, None -> raise (Tag_exception "cannot parse integer in <if higherThan> tag")
              | _, _ -> ()
            end;

            run_if_operation operation variable value children state namespace;

        (* <if ...> *)
        | Xml.Element ("if", _, _) ->
            raise (If_exception ("Invalid if definition"))

        (* <set variable="var" value="foo" /> *)
        | Xml.Element ("set", [("variable", var_name); ("value", value)], []) ->
            (* Abort if variable is not defined before *)
            if not (Hashtbl.mem namespace.var_tbl var_name) then
              raise (Tag_exception "variable not found in <set> tag");

            let value = eval_content value state namespace in
            Hashtbl.replace namespace.var_tbl var_name value;
            ""

        | Xml.Element ("set", [("record", record_name); ("field", field_name); ("value", new_value)], [])
        | Xml.Element ("set", [("card", record_name); ("field", field_name); ("value", new_value)], []) ->
            let record : record = try Hashtbl.find namespace.record_tbl record_name with
              | Not_found -> raise (Tag_exception (sprintf "Found no record with name %s in <set> tag" record_name))
            in
            let value : string = eval_content new_value state namespace in
            Hashtbl.replace record.values field_name value;
            ""

        | Xml.Element ("set", _, _) ->
            raise (Tag_exception "Invalid set definition");

        (* <add to="<type>" name="<name>">...</add> *)
        | Xml.Element ("add", [("to", target_type); ("name", target_name)], children) ->
            begin match target_type with
              | "list" ->
                  let target_list : list_ = try Hashtbl.find namespace.list_tbl target_name with
                    | Not_found -> raise (Tag_exception (sprintf "Found no list to add to with name %s" target_name))
                  in
                  let new_items : list_item list = parse_list_items target_list.list_type children state namespace in
                  if List.length new_items > 0 then begin
                    let new_list : list_ = {target_list with items = target_list.items @ new_items} in
                    Hashtbl.replace namespace.list_tbl new_list.name new_list;
                  end;
                  ()
              | _ -> raise (Tag_exception ("Invalid target type for <add> tag"))
            end;
            ""

        (* Unknown tag or error *)
        | Xml.Element (what, _, _) ->
            raise (Unknown_tag what)

        (* Only string in tag, no XML *)
        | Xml.PCData content ->
            eval_content content state namespace
      end with
        | ex ->
            Io.print_endline (Printexc.to_string ex);
            Io.print_endline (Printexc.get_backtrace ());
            Io.flush ();
            (*string_of_exn ex*)
            exit 1;
      in 
      if String.trim result <> "" then begin
        log_trace "print_tags: printing result";
        Io.print_string result;
        Io.flush ();
      end;
      if result = "\n" then Io.print_endline "";
      ""

  (**
   * Convert all sentences to strings
   * And macros, variables, records, includes...
   *
   * @param story XML
   * @param state
   * @return string
   *)
  and print_sentences (story : Xml.xml) (state : state) namespace =
    log_trace "print_sentences";
    let raw_backtrace = Printexc.get_callstack 20 in
    let raw_s = Printexc.raw_backtrace_to_string raw_backtrace in
    log_trace ("print_sentences raw_s = \n" ^ raw_s);
    let sentences = fetch_children story in
    let string_sentences = List.map (fun s ->
      log_trace "print_sentences: map";
      print_tag s state namespace
    ) sentences in
    let result = List.fold_left (^) "" string_sentences in
    log_trace (sprintf "print_sentences result = %s\n" result);
    String.trim result

  (**
   * Init state with global namespace and hash tables
   *
   * @param unit
   * @return state
   *)
  and init_state () =
    let global_namespace : namespace = new_namespace "global" in
    let namespace_tbl : (string, namespace) Hashtbl.t = (Hashtbl.create 10 : namespace_tbl) in
    Hashtbl.add namespace_tbl "global" global_namespace;
    (* Return state *)
    {
      config = [];
      namespace_tbl;
    }

  (**
   * Return string of XML-file with story etc.
   * Exits if XMl can't be parsed.
   *
   * @param string filename
   * @param state state
   * @return string
   * @TODO: Factor out so this can be used both from command line and web page.
   *)
  and file_to_string filename (state : state) =
    let xml : Xml.xml = try Xml.parse_file filename with
      | Xml.Error (msg, pos) ->
          Io.print_endline (sprintf "Error while parsing XML file '%s'" filename);
          print_int (Xml.line pos);
          Io.print_endline (": " ^ Xml.error_msg msg);
          exit 0;
    in
    let story : Xml.xml = fetch_node xml "story" in
    story_to_string story state

  (**
   * Parse <story> element and output string
   *
   * @param story Xml.xml
   * @param state state
   * @return string
   *)
  and story_to_string story state =
    (* Get possible namespace attribute for <story> *)
    let attrs : (string * string) list            = Xml.attribs story in
    let namespace_attr : (string * string) option = find_attribute attrs "namespace" in
    let newline_attr : (string * string) option   = find_attribute attrs "newline" in
    (* Check for newline config *)
    let state : state = match newline_attr with
      | Some ("newline", "print") -> {state with config = ("newline", "") :: state.config}
      | Some ("newline", s) -> raise (State_exception ("Invalid value for newline attribute: " ^ s))
      | Some (_, _) -> raise (Internal_error "Messed up newline setting")
      | None -> state
    in
    (* Check for top-level namespace *)
    begin match namespace_attr with
      | Some ("namespace", namespace_name) ->
          let namespace = get_namespace_or_create state namespace_name in
          print_sentences story state namespace
      | None ->
          let global_namespace = Hashtbl.find state.namespace_tbl "global" in
          print_sentences story state global_namespace
      | _ ->
          raise (Internal_error "Couldn't find namespace attribute for story tag")
    end

  (** Evaluate AST, part of template lang. TODO: Factor out in own module? How? Interdependencies. *)

  (**
   * @param term
   * @param state state
   * @param namespace namespace option
   * @return string
   *)
  and eval_term (term : Ast.term) (state : state) (namespace : namespace) : string =
    log_trace "eval_term";
    let open Ast in
    match term with
    | Variable var_name ->
        eval_var var_name namespace
    | Record (record_name, value) ->
        eval_record record_name value namespace
    | Macro macro_name ->
        eval_macro macro_name namespace
    | Deck deck_name ->
        eval_deck deck_name state namespace
    | Graph graph_name ->
        eval_graph graph_name state namespace
    | Dice (dice_name, number_of_dice) ->
        eval_dice dice_name number_of_dice namespace
    | Input (variable_name) ->
        eval_input variable_name state namespace
    | Int i ->
        string_of_int i
    | Plus (left, right) ->
        log_trace "eval_term: plus";
        let left : string = eval_term left state namespace in
        log_trace (sprintf "left = %s" left);
        let right : string = eval_term right state namespace in
        log_trace (sprintf "right = %s" right);
        let left : int = try int_of_string left with 
          | Failure _ -> raise (Ast_exception ("Plus: Could not get int of string from left value: " ^ left))
        in
        let right : int = try int_of_string right with
          | Failure _ -> raise (Ast_exception ("Plus: Could not get int of string from right value: " ^ right))
        in
        string_of_int (left + right)
    | Minus (left, right) ->
        let left = eval_term left state namespace in
        let right = eval_term right state namespace in
        let left : int = try int_of_string left with 
          | Failure _ -> raise (Ast_exception ("Minus: Could not get int of string from left value: " ^ left))
        in
        let right : int = try int_of_string right with
          | Failure _ -> raise (Ast_exception ("Minus: Could not get int of string from right value: " ^ right))
        in
        string_of_int (left - right)

  (**
   * Eval nameterm
   *
   * @param nameterm nameterm
   * @param state state
   * @param default_namespace namespace
   * @return string
   *)
  and eval_nameterm nameterm state default_namespace =
    let open Ast in
    match nameterm with
    | Nameterm (namespace_name, term) ->
        let namespace = get_namespace state namespace_name in
        begin match namespace with
        | Some namespace ->
            eval_term term state namespace
        | None ->
            raise (Eval_ast_exception (sprintf "Found no namespace with name '%s'" namespace_name))
        end
    | Term term ->
        eval_term term state default_namespace
    | Content content ->
        log_trace (sprintf "eval_nameterm: content = %s " content);
        let content_without_quotes = String.sub content 1 (String.length content - 2) in
        content_without_quotes

  (**
   * Eval AST for inline template language
   *
   * @param ast ast
   * @param dice int -> int
   * @param state state
   * @param default_namespace namespace
   * @return string
   *)
  and eval_ast ast state default_namespace : string =
    let open Ast in
    match ast with
    | Nameterm_list nameterms ->
        let length = List.length nameterms in
        begin match List.nth nameterms (Dice.dice length) with
        | nameterm ->
            eval_nameterm nameterm state default_namespace
        | exception Not_found ->
            raise (Eval_ast_exception "Could not chose a nameterm")
        end

  (**
   * Lex and parse ast.
   * @param match_ preg match for {foo}
   * @return ast
   *)
  and parse_ast match_ =
    let linebuf = Lexing.from_string match_ in
    let ast = try (Tparser.main Tlexer.token linebuf) with
      | Tlexer.Error msg ->
          (*
          let tok = Lexing.lexeme linebuf in
          *)
          raise (Parser_error (sprintf "%s" msg))
      | Tparser.Error ->
          (*
          let tok = Lexing.lexeme linebuf in
          *)
          raise (Parser_error (sprintf "Could not parse '%s': error at %c" match_ (String.get match_ (Lexing.lexeme_start linebuf))))
          (*raise (Parser_error (sprintf "Could not parse '%s'" match_ ))*)
      | Failure msg ->
          let open Lexing in
          raise (Internal_error (sprintf "line = %d; col = %d" linebuf.lex_curr_p.pos_lnum linebuf.lex_curr_p.pos_cnum))
    in
    ast

  (**
   * Eval <story> tag and all its children (macros, records, etc), returns the string
   *
   * @param story Xml.Element
   * @return string
   *)
  let eval_story story =
    ()

  let get_story =
    ()

end
