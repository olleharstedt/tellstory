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
exception Sentence_problem of string * exn

(* Hash table to store flags, with flag name as key *)
let flags_tbl = ((Hashtbl.create 20) : ((string, bool) Hashtbl.t))

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
    | Xml.Element (_, [attr, str], _) ->  raise (Illegal_attribute_name attr)
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

(**
 * Print a sentence with random alt.
 *
 * @param sentence Xml.xml
 * @return string
 *)
let print_sentence sentence =
  let sen = String.trim (fetch_content (sentence)) in
  try (
    let alt = choose_alt (fetch_nodes (sentence) "alt") in
    match alt with
      | None -> sen
      | Some alt ->
        let flags_list = get_flags alt in
        store_flags flags_list;
        let cont = fetch_content alt in
        sen ^ cont
  )
  with 
    ex -> 
      raise (Sentence_problem (sen, ex))

(**
 * Convert all sentences to strings
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
