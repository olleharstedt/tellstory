(*
 * Render random texts from XML-file.
 *
 * @since 2015-03-05
 * @author Olle Härstedt
 *)

open Printf

exception No_node_content of string
exception Not_implemented
exception No_such_lang of string
exception No_such_direct_node of string
exception No_filename
exception Too_many_attributes of string
exception Flag_already_set of string
exception Illegal_attribute_name of string
exception Error_parsing_xml
exception Unknown_tag of string
exception Sentence_problem of string * exn
exception Macro_exception of string
exception Variable_exception of string
exception AltException of string

(** Data types for storing macros *)
type macro_alt = {
  content : string
}

type macro = {
  name : string;
  alts : macro_alt list
}

(** Hash table to store macros. Macros are randomized each use. *)
let macro_tbl = ((Hashtbl.create 20) : ((string, macro) Hashtbl.t))

(* Hash table to store flags, with flag name as key. Flags are used for branching. *)
let flags_tbl = ((Hashtbl.create 20) : ((string, bool) Hashtbl.t))

(* Hash table to store variables. Variables are only randomized once. *)
let vars_tbl = ((Hashtbl.create 20) : ((string, string) Hashtbl.t))

(* List of attributes allowed in <sentence> tag *)
let allowed_sentence_attributes = ((Hashtbl.create 5) : ((string, bool) Hashtbl.t))
let _ =
  Hashtbl.add allowed_sentence_attributes "ifFlagIsSet" true

(* List of attributes allowed in <alt> tag *)
let allowed_alt_attributes = ((Hashtbl.create 5) : ((string, bool) Hashtbl.t))
let _ =
  Hashtbl.add allowed_alt_attributes "setFlag" true


(** Return number from 1 to n *)
let dice n =
  Random.int n + 1

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
    Xml.Element (tag, attrs, []) -> assert false
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
 * Get ifFlagIsSet condition for sentence
 *
 * @param xml sent          Sentence
 * @return string option    flag name
 *)
let getFlagCondition sent =
  ()

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
 * Choose one of the alt:s in a sentence.
 *
 * @param sentence Xml.xml list, first element is actual sentence, rest is alt:s
 * @return xml option
 *)
let choose_alt sentence = match sentence with
  | Xml.PCData _ :: [] -> None
  | Xml.PCData _ :: only_one :: []-> Some only_one
  | Xml.PCData _ :: tail ->
      let nr = (List.length tail) in
      Some (List.nth tail (dice nr - 1))
  | all_xmls ->
      let nr = (List.length all_xmls) in
      Some (List.nth all_xmls (dice nr - 1))

(**
 * Check if alt has a flag attribute and return it
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
 * @param flags_list string list
 * @return unit
 *)
let store_flags flags_list = match flags_list with
    None -> ()
  | Some fl ->
    List.iter (fun s ->
      (* Check if flag is already set. If so, abort *)
      if Hashtbl.mem flags_tbl s then
        raise (Flag_already_set s)
      else
        Hashtbl.add flags_tbl s true
    ) fl

(** Parse macro alts
 *
 * @param alts Xml.Element list
 * @return alt list
 *)
let parse_macro_alts alts =
  List.map (fun alt -> match alt with
    | Xml.Element ("alt", [], [Xml.PCData content]) ->
        {content}
    | _ ->
        raise (Macro_exception "Illegal alt in macro")
  ) alts

(** Parse macro *)
let parse_macro xml = match xml with
  | Xml.Element ("macro", [("name", name)], alts) ->
      let alts = parse_macro_alts alts in
      {name; alts}
  | Xml.Element ("macro", [], _) ->
      raise (Macro_exception "Macro has no name attribute")
  | Xml.Element ("macro", _, []) ->
      raise (Macro_exception "Macro has no alts")
  | _ ->
      raise (Macro_exception "Macro definition is fucked, yo")

(** Store macro in macro hash table. Raise exception if macro with this name
 * already exists.
 *)
let store_macro macro =
  if Hashtbl.mem macro_tbl macro.name then
    raise (Macro_exception ("Macro with name " ^ macro.name ^ " already exists"))
  else
    Hashtbl.add macro_tbl macro.name macro

(**
 * Eval <alt> to its content
 *
 * @param alt Xml.Element
 * @return string
 *)
let eval_alt alt =
  (* Aux function to check for <alt></alt> *)
  let check_for_empty_content c =
    match c with
    | "" ->
        raise (AltException "Empty content for <alt>. Check so no <alt> is defined as <alt></alt>")
    | _ ->
        ()
  in
  match alt with
  | Xml.Element ("alt", [("useMacro", macro_name)], _) ->
      if Hashtbl.mem macro_tbl macro_name then begin
        let macro = Hashtbl.find macro_tbl macro_name in
        let alts = macro.alts in
        let alt = List.nth alts (dice (List.length alts) - 1) in
        check_for_empty_content alt.content;
        alt.content
      end else
        raise (Macro_exception ("useMacro: Found no macro with name " ^ macro_name))
  | _ ->
      let flags_list = get_flags alt in
      store_flags flags_list;
      let content = fetch_content alt in
      check_for_empty_content content;
      content

(**
 * Eval sentence, changin {bla} to variable content
 *
 * @param sen string
 * @return string
 *)
let eval_sen sen =
  try (
    let matches = Pcre.exec_all ~pat:"{[a-zA-Z]+}" sen in
    let matches = Array.to_list (
      Array.map (fun m ->
        let substrings = Pcre.get_substrings m in
        let s = substrings.(0) in
        let s = String.sub s 1 (String.length s - 2) in
        s
      ) matches;
    ) in
    let matches_uniq = List.sort_uniq (fun a b -> 1) matches in
    (* Aux function to replace matches with found variables *)
    let rec replace matches sen = match matches with
      | [] -> sen
      | x::xs -> 
          let pattern = "{" ^ x ^ "}" in
          let replacement = try 
              Hashtbl.find vars_tbl x 
            with 
              Not_found -> raise (Variable_exception (sprintf "Could not find variable with name '%s'" x))
          in
          let sen = Pcre.replace ~pat:pattern ~templ:replacement sen in
          replace xs sen
    in
    replace matches_uniq sen
  )
  with
    Not_found -> sen
  (* Get all {foo} in str *)
  (* Loop through each, replace with variable content *)
  (* Get all {foo.bar} in str *)
  (* Loop and replace *)

(**
 * Print a sentence with random alt.
 *
 * @param sentence Xml.xml
 * @return string
 *)
let print_sentence sentence =
  let sen = String.trim (fetch_content (sentence)) in
  let sen = eval_sen sen in
  try (
    let alt = choose_alt (fetch_nodes (sentence) "alt") in
    match alt, sen with
      | None, _ -> sen
      | Some alt, "" ->
          (eval_alt alt)
      | Some alt, sen ->
          sen ^ " " ^ (eval_alt alt)
          (* TODO: If alt is ""? *)
  )
  with
    ex ->
      raise (Sentence_problem (sen, ex))

(**
 * Check so that only Xml.Element "alt" are in the list
 *
 * @param alts Xml.Element list
 * @return bool
 *)
let only_alts alts =
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
let variable_name_free name =
  not (Hashtbl.mem vars_tbl name)

(**
 * Check if all <alt>:s have a content (except macro alts)
 *
 * @param alts Xml.Element list
 * @return bool
 *)
let all_alts_have_content alts =
  List.for_all (fun a -> match a with
  | Xml.Element ("alt", _, [Xml.PCData content]) ->
      true
  | Xml.Element ("alt", [("useMacro", macro_name)], _) ->
      true
  | Xml.Element ("alt", _, _) ->
      false
  | _ ->
      false
  ) alts


(**
 * Check if we can add new variable with alts and name
 *
 * @param name string Name of new variable
 * @param alts Xml.Element list
 * @return bool
 *)
let variable_is_ok name alts =
  if not (only_alts alts) then
    raise (Variable_exception ("Only <alt> allowed in variable tag for variable " ^ name))
  else if not (all_alts_have_content alts) then
    raise (Variable_exception ("Some <alt> in variable with name '" ^ name ^ "' does not have any content"))
  else if not (variable_name_free name) then
    raise (Variable_exception ("<variable> with name '" ^ name ^ "' is already in use, can only be defined once."))
  else
    true

(**
 * Eval <variable>
 *)
let eval_variable name alts =
  let alt = List.nth alts (dice (List.length alts) - 1) in
  let content = eval_alt alt in
  Hashtbl.add vars_tbl name content

(**
 * Convert all sentences to strings
 * And macros, variables, records, includes...
 *
 * @param story XML
 * @return string
 *)
let print_sentences story =
  let sentences = fetch_children story in
  let string_sentences = List.map (fun s ->
    let sen = String.trim (fetch_content s) in
    match s with
      | Xml.Element ("sentence", [("ifFlagIsSet", flags)], _) ->
          (* All flags in list must be set to print sentence *)
          let flag_list = Str.split (Str.regexp "[ \t]+") flags in
          let all_flags_are_set = List.for_all (fun flag ->
            Hashtbl.mem flags_tbl flag
          ) flag_list in
          if all_flags_are_set then
            (print_sentence s) ^ " "
          else
            ""
      | Xml.Element ("sentence", [attr, flags], _) ->
          raise (Sentence_problem (sen, Illegal_attribute_name attr))
      | Xml.Element ("sentence", x::xs, _) ->
          raise (Sentence_problem (sen, (Too_many_attributes ("sentence"))))
      | Xml.Element ("sentence", [], _) -> (print_sentence s) ^ " "
      | Xml.Element ("br", _, _) -> "\n\n"
      | Xml.Element ("macro", _, _) ->
          (* store macro *)
          let macro = parse_macro s in
          store_macro macro;
          "" (* Return empty string *)

      (* <variable> *)
      | Xml.Element ("variable", [("name", name)], alts) when (variable_is_ok name alts)->
          eval_variable name alts;
          ""

      (* Unknown tag or error *)
      | Xml.Element (what, _, _) -> raise (Sentence_problem (sen, Unknown_tag what))
      | _ -> raise (Sentence_problem (sen, Error_parsing_xml))
  ) sentences in
  List.fold_left (^) "" string_sentences

(* Main *)
let _ =

  let filename = if Array.length Sys.argv == 2 then Sys.argv.(1) else raise No_filename in

  Random.self_init ();

  let xml = try Xml.parse_file filename with
    | Xml.Error (msg, pos) ->
        print_endline ("Error while parsing XML file '" ^ filename ^ "'");
        print_int (Xml.line pos);
        print_endline (": " ^ Xml.error_msg msg);
        exit 0;
  in
  let story = fetch_node xml "story" in
  let string_story = try (
    print_sentences story
  )
  with
    | Sentence_problem (sen, ex) ->
      print_endline ("Problem with sentence '" ^ sen ^ "'");
      raise ex
  in
  print_endline "";
  print_endline string_story;
  print_endline ""
