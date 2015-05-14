(*
 * Render random texts from XML-file.
 *
 * @since 2015-03-05
 * @author Olle Härstedt
 *)

open Printf
open Core
open Core_list

(**
 * Debug with Bolt
 *
 * Run with BOLT_FILE=bolt.config ./tellstory
 *
 *)
let log_trace msg =
  Bolt.Logger.log "tellstory_debug_logger" Bolt.Level.TRACE msg

(**
 * Need to factor out dice function because in tests we want to control
 * return values.
 *
 * @since 2015-03-26
 *)
module type D = sig
  val dice : int -> int
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
  val init_state : unit -> state
  val get_global_namespace : state -> namespace
end

module Make(Dice : D) : T = struct
  exception No_node_content of string
  exception Not_implemented
  exception Internal_error of string
  exception No_such_lang of string
  exception No_such_direct_node of string
  exception Too_many_attributes of string
  exception Flag_already_set of string
  exception Illegal_attribute_name of string
  exception Error_parsing_xml
  exception Unknown_tag of string
  exception Sentence_problem of string * string (* Message, string of exn *)
  exception Macro_exception of string
  exception Variable_exception of string
  exception Record_exception of string
  exception Alt_exception of string
  exception Parser_error of string
  exception Deck_exception of string
  exception Xml_exception of string
  exception Include_exception of string * exn
  exception Namespace_exception of string

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

  (** Data types for storing decks *)

  type deck = {
    name : string;
    alts : alt list;  (* When an alt is chosen it's removed from list (card is picked). Remove and replace in deck hashtable? *)
  }

  (** Types for storing records *)
  type record = (string, string) Hashtbl.t

  (** Hash table to store macros. Macros are randomized each use. *)
  (*
  let macro_tbl = ((Hashtbl.create 20) : ((string, macro) Hashtbl.t))

  (** Hash table to store variables. Variables are only randomized once. *)
  let var_tbl = ((Hashtbl.create 20) : ((string, string) Hashtbl.t))

  (** Hash table to store records. Randomized once. *)
  let record_tbl = ((Hashtbl.create 20) : ((string, record) Hashtbl.t))

  (** Hash table for decks *)
  let deck_tbl = ((Hashtbl.create 20) : ((string, deck) Hashtbl.t))
  *)

  (*
  type namespace_element =
  | Macro_tbl of (string, macro) Hashtbl.t
  | Var_tbl of (string, string) Hashtbl.t
  | Record_tbl of (string, record) Hashtbl.t
  | Deck_tbl of (string, deck) Hashtbl.t
  *)

  type macro_tbl = (string, macro) Hashtbl.t
  type var_tbl = (string, string) Hashtbl.t
  type deck_tbl = (string, deck) Hashtbl.t
  type record_tbl = (string, record) Hashtbl.t

  (** Namespace of macros, variables, ... *)
  type namespace = {
    name : string;
    macro_tbl : macro_tbl;
    var_tbl : var_tbl;
    record_tbl : record_tbl;
    deck_tbl : deck_tbl;
  }

  type namespace_tbl = (string, namespace) Hashtbl.t

  (** Explicit state, pass around every where... *)
  type state = {
    namespace_tbl : namespace_tbl;
  }

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


  let print_deck (deck : deck) =
    log_trace (sprintf "%s\n" deck.name);
    log_trace "alts:\n";
    iter deck.alts ~f:(fun alt ->
      if length alt.attributes > 1 then raise (Alt_exception "More then 1 attributes for alt: can't print");
      let attr = nth alt.attributes 0 in
      match attr with
      | None ->
          log_trace (sprintf "<alt>%s</alt>" alt.content)
      | Some attr ->
          log_trace (sprintf "<alt %s='%s'>%s</alt>\n" (fst attr) (snd attr) alt.content)
    )

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
    | Xml.Element (tag, _, _) -> "" (* TODO: Abort here or not? *)
    (*| Xml.Element (tag, _, _) -> raise (No_node_content tag)*)

  (**
   * Fetch the content of an Xml node
   *
   * @param xml Xml.xml
   * @param node string
   * @return string
   *)
  let fetch_node_content xml node =
    let node = fetch_node xml node in
    fetch_content node

  (**
   * Aux function to find attribute
   * Find first attribute with name
   *
   * @param attributes ? Xml.Element list
   * @param name string
   * @return Xml.Element option???
   *)
  let find_attribute attributes name =
    find attributes ~f:(function
      | (attr_name, _) ->
          name = attr_name
    )

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
      var_tbl = ((Hashtbl.create 20) : var_tbl);
      macro_tbl = ((Hashtbl.create 20) : macro_tbl);
      record_tbl = ((Hashtbl.create 20) : record_tbl);
      deck_tbl = ((Hashtbl.create 20) : deck_tbl);
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
      log_trace (sprintf "new namespace created with name '%s'" name);
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
   * Get list of possible alts to consider, conserning flags
   *
   * @param alts Xml.Element list
   * @return Xml.Element list
   *)
  let get_possible_alts alts = match alts with
    | [] ->
        []
    | alts ->
        (* Filter alts that don't belong *)
        let alts2 = filter alts ~f:(fun alt ->
          (* Have to check for flag condition to rule out some alts *)
          let attrs = Xml.attribs alt in
          let ifSet = find_attribute attrs "ifSet" in
          (* Did we find ifSet? *)
          begin match ifSet with
            | None ->
                (* No flag condition? Ey ok. *)
                true
            | Some ("ifSet", flags) ->

                let split_flags = (Str.split (Str.regexp "[ \t]+") flags) in

                (* Check so all flags are set in hash table. *)
                for_all split_flags ~f:(fun flag ->
                  Hashtbl.mem Globals.flags_tbl flag
                )
            | _ ->
                raise (Internal_error "get_possible_alts: Illegal struct of ifSet")
          end
        ) in
        (*(iter alts2 ~f:(fun xml -> log_trace "alt = %s\n" (Xml.to_string xml)));*)
        alts2

  (**
   * Chose an alt to use, depending on flags
   *
   * @param alts Xml.Element list
   * @return Xml.Element option
   *)
  let choose_alt alts =
    let possible_alts = get_possible_alts alts in
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
      nth possible_alts (Dice.dice nr)

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
  let get_flags alt =
    let flags = match alt with
      | Xml.Element (_, ["setFlag", str], _) ->  Some str
      (*| Xml.Element (_, [attr, str], _) ->  raise (Illegal_attribute_name attr)*)
      | Xml.Element (_, x::xs, [Xml.PCData alt_content]) -> raise (Too_many_attributes ("alt: " ^ alt_content))
      | _ -> None
    in
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
  let store_flags flags_list = match flags_list with
      None -> ()
    | Some fl ->
      List.iter (fun s ->
        (* Check if flag is already set. If so, abort *)
        if Hashtbl.mem Globals.flags_tbl s then
          raise (Flag_already_set s)
        else
          Hashtbl.add Globals.flags_tbl s true
      ) fl

  (**
   * Parse macro/deck xml alts as alt list
   *
   * @param alts Xml.Element list
   * @return alt list
   *)
  let parse_alts alts =
    map alts ~f:(fun alt -> match alt with
      | Xml.Element ("alt", attributes, [Xml.PCData content]) ->
          {content; attributes}
      | Xml.Element ("alt", attributes, []) ->
          {content = ""; attributes}
      | _ ->
          raise (Alt_exception "Illegal alt in macro/deck")
    )

  (** 
   * Parse macro tag
   *
   * @param xml Xml.xml
   * @return macro
   *)
  let parse_macro (xml) (state : state) : (macro * namespace option) = match xml with
    | Xml.Element ("macro", attrs, alts) ->
        let namespace_name = find_attribute attrs "namespace" in
        let macro_name = find_attribute attrs "name" in
        begin match macro_name, namespace_name with
        | Some ("name", macro_name), Some ("namespace", namespace_name) ->
            let alts = parse_alts alts in
            let namespace = get_namespace_or_create state namespace_name in
            ({name = macro_name; alts}, Some namespace)
        | Some ("name", macro_name), _ ->
            let alts = parse_alts alts in
            ({name = macro_name; alts}, None)
        | None , _ ->
            raise (Macro_exception "Macro has no name attribute")
        | _, _ ->
            raise (Internal_error "Macro definition is super weird")
        end
    | _ ->
        raise (Internal_error "Macro definition is weird")

  (** 
   * Store macro in macro hash table. Raise exception if macro with this name
   * already exists.
   *
   * @param macro
   * @param macro_tbl hash table
   * @return unit
   *)
  let store_macro (macro : macro) namespace =
    if Hashtbl.mem namespace.macro_tbl macro.name then
      raise (Macro_exception (sprintf "Macro with name '%s' already exists" macro.name))
    else
      Hashtbl.add namespace.macro_tbl macro.name macro


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
          raise (Alt_exception "Empty content for <alt>. Check so no <alt> is defined as <alt></alt>")
      | _ ->
          ()
    in

    if Hashtbl.mem namespace.macro_tbl name then begin
      let macro = Hashtbl.find namespace.macro_tbl name in
      (*let alt = choose_alt macro.alts in*)
      let alts = macro.alts in
      let alt = match nth alts (Dice.dice (List.length alts)) with
        | Some alt -> alt
        | None -> raise (Macro_exception "No alt?")
      in
      check_for_empty_content alt.content;
      alt.content
    end else
      raise (Macro_exception (sprintf "useMacro: Found no macro '%s' in namespace '%s'" name namespace.name))

  (**
   * Choose card from deck and remove it from the deck.
   * Return card content.
   *
   * @param deck_name string
   * @param state state
   * @param namespace namespace
   * @return string
   *)
  let rec eval_deck deck_name (state : state) (namespace : namespace) =
    log_trace "eval_deck\n";
      let deck = try Hashtbl.find namespace.deck_tbl deck_name with
      | Not_found -> raise (Deck_exception (sprintf "Found no deck with name '%s'." deck_name))
      in
      (* Abort if no cards are left in deck *)
      if length deck.alts = 0 then raise (Deck_exception (sprintf "No more cards in deck '%s'." deck_name));
      let card = nth deck.alts (Dice.dice (length deck.alts)) in
      match card with
      | None -> raise (Deck_exception ("Found no card"))
      | Some card -> 
          (* Create a new deck without the card we picked *)
          (* Card = alt in this context *)
          (* Filter logic:
           * We want to filter the card we just picked.
           * All cards that are not identical to this card should return false (not filter)
           * The card is identical to the card we picked IF:
           *   content = content AND attributes are equal (name && content of attribues are equal)
           *)
          let new_alts = filter deck.alts ~f:(fun alt ->
            not (compare_alt alt card)
          ) in
          let new_deck = {
            name = deck.name;
            alts = new_alts;
          } in
          log_trace "old_deck: ";
          print_deck deck;
          log_trace "new deck: ";
          print_deck new_deck;
          Hashtbl.remove namespace.deck_tbl deck.name;
          Hashtbl.add namespace.deck_tbl new_deck.name new_deck;
          (* Build Xml.xml <alt> out of record *)
          let alt = Xml.Element ("alt", card.attributes, [Xml.PCData card.content]) in
          let result = eval_alt alt state namespace in
          log_trace (sprintf "eval_deck: result of eval_alt = %s\n" result);
          result

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
    else begin match hd matches with
    | None ->
        raise (Internal_error (sprintf "get_inline_namespace: Found one namespace, but no match?"))
    | Some namespace_name ->
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

    let var_tbl = namespace.var_tbl in

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
      | x::xs ->
          let pattern = "{" ^ x ^ "}" in
          let replacement = try
              Hashtbl.find var_tbl x
            with
              Not_found -> raise (Variable_exception (sprintf "Could not find variable '%s' in namespace '%s'. Check that you defined it with <variable name=\"%s\">..." x namespace.name x))
          in
          let content = Pcre.replace ~pat:pattern ~templ:replacement content in
          replace_variables xs content
    in
    replace_variables matches_uniq content

  (**
   * Replace inline records like {this.that}
   *
   * @param content string Content
   * @param record_tbl hash table
   * @return string
   *)
  and replace_inline_records content record_tbl =
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
          let record = try Hashtbl.find record_tbl record_name with
            Not_found -> raise (Record_exception (sprintf "No such record: '%s'" record_name))
          in
          let var = try Hashtbl.find record var_name with
            Not_found -> raise (Record_exception (sprintf "No such tag '%s' in record '%s'" var_name record_name))
          in
          let pattern = sprintf "{%s.%s}" record_name var_name in
          let content = Pcre.replace ~pat:pattern ~templ:var content in
          replace_records xs content
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
    let rec replace_macros matches content = match matches with
    | [] -> content
    | mat::tail ->
        let macro_content = eval_macro mat namespace in
        let pattern = sprintf "{#%s}" mat in
        let content = Pcre.replace ~pat:pattern ~templ:macro_content content in
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
    let matches = try Pcre.exec_all ~pat:"{\"[a-zA-Z0-9_\\s]+\"}" con with Not_found -> [||] in
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
   * @param con string
   * @return string
   *)
  and eval_content con state namespace =
    let con = replace_inline_content con in
    let con = replace_inline_variable con state namespace in
    let con = replace_inline_records con namespace.record_tbl in
    let con = replace_inline_macros con state namespace in
    let con = replace_inline_deck con state namespace in

    (* Replace inline randomization like {this | and_this.too | #and_that} (without space - ppx problem) *)
    (*let matches = try Pcre.exec_all ~pat:"{([a-zA-Z0-9_#\\.]+|\"[a-z]+\"|\\|)}" con with Not_found -> [||] in*)
    let matches = try Pcre.exec_all ~pat:"{[a-zA-Z0-9_\\|#\\.\\\"\\s]+}" con with Not_found -> [||] in
    let matches = Array.to_list matches in
    (* Get tuples like what to match and what to replace it with: (match, replace_with_this) *)
    let matches_and_replaces = map matches ~f:(fun m -> 
      let substrings = Pcre.get_substrings m in
      let substrings_tuples = ArrayLabels.map substrings ~f:(fun substring ->
        (* Strip {} *)
        let no_braces = String.sub substring 1 (String.length substring - 2) in
        let split_by_bar = Pcre.split ~pat:"\\|" no_braces in
        let alt = match nth split_by_bar (Dice.dice (List.length split_by_bar)) with
          | Some alt -> alt
          | None -> raise (Internal_error (sprintf "eval_content: found no alternative when splitting '%s'" substring))
        in
        (*
        iter split_by_bar ~f:(fun s ->
          print_endline s;
          print_endline (eval_content (sprintf "{%s}" s))
        );
        *)
        (substring, eval_content (sprintf "{%s}" alt) state namespace)
      ) in
      substrings_tuples.(0)
    ) in
    let rec replace_randomization matches con = match matches with
    | [] -> con
    | (mat, repl)::tail ->
        let mat = Pcre.quote mat in
        let con = Pcre.replace_first ~pat:mat ~templ:repl con in
        replace_randomization tail con
    in
    let con = replace_randomization matches_and_replaces con in

    (*
    iter matches_and_replaces ~f:(fun (x, y) ->
      print_endline x;
      print_endline y
    );
    *)

    con

  (**
   * Eval <alt> to its content
   *
   * @param alt Xml.Element
   * @param state state
   * @return string
   *)
  and eval_alt alt (state : state) (namespace : namespace) =
    let attributes = Xml.attribs alt in

    (* Find attribute setFlag *)
    let setFlag = find_attribute attributes "setFlag" in
    let flags_list = match setFlag with
      | None ->
          None
      | Some (_, flags) ->
         Some (Str.split (Str.regexp "[ \t]+") flags)
    in
    store_flags flags_list;

    (* Get macro to use as (string * string) tuple *)
    let useMacro = find_attribute attributes "useMacro" in
    let useDeck = find_attribute attributes "useDeck" in
    let _include = find_attribute attributes "include" in

    (* Get content either from macro, deck or alt.content *)
    let cont = match useMacro, useDeck, _include with
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
   *
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
   * Check if record definition is valid
   *
   * @param name string
   * @param alts Xml.Element list
   * @return bool
   *)
  and record_is_ok name alts record_tbl =
    (* Check so record name is free *)
    if Hashtbl.mem record_tbl name then begin
      log_trace "record_is_ok";
      log_trace (Printexc.get_backtrace ());
      raise (Record_exception (sprintf "Record name '%s' is already in use" name))
    end;

    (* Check so alts are not empty *)
    if List.length alts = 0 then begin
      raise (Record_exception (sprintf "Record '%s' has no <alt>" name))
    end;

    (* Each alt must have equal number of children with same structure *)
    let compare alts = match alts with
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
    if not (compare alts) then begin
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
   * Eval <variable>
   * Adds variable to variable hash table
   *
   * @param name string
   * @param alts Xml.Element list
   * @param state state
   * @return void
   *)
  and eval_variable name alts (state : state) namespace =
    match choose_alt alts with
    | None ->
        raise (Variable_exception (sprintf "No alt could be chosen for variable '%s'" name))
    | Some alt ->
      let content = eval_alt alt state namespace in
      Hashtbl.add namespace.var_tbl name content

  (**
   * Adds record to record hash table
   *
   * @param name string
   * @param alts Xml.Element list
   * @param record_tbl record_tbl
   * @return void
   *)
  and eval_record name alts record_tbl =

    (* Choose alt *)
    let alt = match nth alts (Dice.dice (List.length alts)) with
      | Some alt -> alt
      | None -> raise (Record_exception "No alt?")
    in

    (* Store flags if present *)
    let flags_list = get_flags alt in
    store_flags flags_list;

    (* Store vars from alt in record hash table *)
    let record = Hashtbl.create 20 in
    List.iter (fun var ->
      match var with
      | Xml.Element (name, [], [Xml.PCData content]) ->
          Hashtbl.add record name content
      | Xml.Element (var_name, _, []) ->
          raise (Record_exception (sprintf "No empty content allowed in '%s' for record '%s'" var_name name))
      | Xml.Element (var_name, _, _) ->
          raise (Record_exception (sprintf "No attributes allowed in '%s' for record '%s'" var_name name))
      | _ ->
          raise (Record_exception (sprintf "Unknown error in record '%s'" name))
    ) (Xml.children alt);

    (* Add record to records hash table *)
    Hashtbl.add record_tbl name record

  (**
   * Parse an xml structure as deck
   *
   * @param xml Xml.xml
   * @return deck
   *)
  (*
  let parse_deck xml : deck = match xml with
    | _ -> ()
  *)

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
   * Parses ifSet="" and return the result
   *
   * @param attrs Xml.Element list
   * @return bool
   * @raise Errors if parse failes
   *)
  and flags_is_ok attrs =
    let ifSet = find_attribute attrs "ifSet" in
    match ifSet with
    | None ->
        true
    | Some ("ifSet", flags) ->
        let lexing = Lexing.from_string flags in
        let flags_ok = try Parser.main Lexer.token lexing with
          | Lexer.Error msg ->
              raise (Parser_error (sprintf "ifSet: Lexer error %s" msg))
          | Parser.Error ->
              raise (Parser_error (sprintf "ifSet: Syntax error at offset %d" (Lexing.lexeme_start lexing)))
          | Failure msg ->
              let open Lexing in
              raise (Internal_error (sprintf "line = %d; col = %d" lexing.lex_curr_p.pos_lnum lexing.lex_curr_p.pos_cnum))
        in
        flags_ok
    | _ ->
        raise (Internal_error "flags_is_ok: Weird result from find_attribute")

  (**
   * Convert all sentences to strings
   * And macros, variables, records, includes...
   *
   * @param story XML
   * @param state
   * @return string
   *)
  and print_sentences story (state : state) namespace =
    log_trace "print_sentences";
    let raw_backtrace = Printexc.get_callstack 20 in
    let raw_s = Printexc.raw_backtrace_to_string raw_backtrace in
    log_trace ("print_sentences raw_s = \n" ^ raw_s);
    let sentences = fetch_children story in
    let string_sentences = map sentences ~f:(fun s ->
      log_trace "print_sentences: map";
      let sen = String.trim (fetch_content s) in
      try begin match s with

        (* <sentence attr="...">...</sentence> *)
        (* Never create namespace when using <sentence>, only get *)
        | Xml.Element ("sentence", attrs, _) when (flags_is_ok attrs) ->
            let namespace_name = find_attribute attrs "namespace" in
            begin match namespace_name with
            | None ->
              print_sentence s state namespace ^ " "
            | Some ("namespace", namespace_name) ->
                let local_namespace = get_namespace state namespace_name in
                begin match local_namespace with
                | None ->
                    let local_namespace = create_namespace state namespace_name in
                    print_sentence s state local_namespace ^ " "
                | Some local_namespace ->
                    print_sentence s state local_namespace ^ " "
                end
            | Some (attr, _) ->
                raise (Sentence_problem (sen, sprintf "Unknown attritube: %s" attr))
            end
        | Xml.Element ("sentence", [], _) ->
            print_sentence s state namespace ^ " "
        (* Empty sentence *)
        | Xml.Element ("sentence", _, _) ->
            ""

        (* <br /> *)
        | Xml.Element ("br", _, _) ->
            "\n\n"

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
        | Xml.Element ("macro", _, _) ->
            (* store macro *)
            let (macro, namespace_option) = parse_macro s state in
            begin match namespace_option with
            | Some namespace ->
                store_macro macro namespace;
            | None ->
                store_macro macro namespace;
            end;
            "" (* Return empty string *)

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
                  eval_variable name alts state namespace
              | Some ("name", name), Some ("namespace", namespace_name) ->
                  (* Don't use default namespace *)
                  let local_namespace = get_namespace_or_create state namespace_name in
                  variable_is_ok name alts local_namespace;
                  eval_variable name alts state local_namespace
              | _, _ ->
                  raise (Variable_exception (sprintf "Attributes wrong for variable"))
            end;
            ""

        (* <record> *)
        | Xml.Element ("record", [("name", name)], alts) when (record_is_ok name alts namespace.record_tbl) ->
            eval_record name alts namespace.record_tbl;
            ""

        (* <deck> *)
        | Xml.Element ("deck", [("name", name)], alts) when (deck_is_ok name alts namespace.deck_tbl) ->
            log_trace "print_sentences: store deck";
            let alts = parse_alts alts in
            store_deck {name; alts} namespace.deck_tbl;
            ""

        (* <include file=""> *)
        | Xml.Element ("include", [("file", filename)], []) ->
            (* TODO: Copied from main.ml, factorize? *)
            (try file_to_string filename state with
            | ex ->
                raise (Include_exception ("Can't include file", ex))
            )

        (* Unknown tag or error *)
        | Xml.Element (what, _, _) -> 
            raise (Unknown_tag what)
        | _ -> 
            raise (Sentence_problem (sen, string_of_exn Error_parsing_xml))
      end with
        | ex ->
            log_trace "exception in print_sentences";
            log_trace (Printexc.to_string ex);
            log_trace (Printexc.get_backtrace ());
            (*raise (Sentence_problem (sen, string_of_exn ex))*)
            print_endline (string_of_exn ex);
            exit 0
    ) in
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
    let global_namespace = new_namespace "global" in
    let namespace_tbl = (Hashtbl.create 10 : namespace_tbl) in
    Hashtbl.add namespace_tbl "global" global_namespace;
    (* Return state *)
    {namespace_tbl}

  (**
   * Return string of XML-file with story etc.
   * Exits if XMl can't be parsed.
   *
   * @param string filename
   * @param state state
   * @return string
   *)
  and file_to_string filename (state : state) =

    let xml = try Xml.parse_file filename with
      | Xml.Error (msg, pos) ->
          print_endline (sprintf "Error while parsing XML file '%s'" filename);
          print_int (Xml.line pos);
          print_endline (": " ^ Xml.error_msg msg);
          exit 0;
    in
    let story = fetch_node xml "story" in
    let global_namespace = Hashtbl.find state.namespace_tbl "global" in
    print_sentences story state global_namespace

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
