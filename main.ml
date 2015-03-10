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
exception Sentence_problem of string * exn

(* Hash table to store flags, with flag name as key *)
let flags = ((Hashtbl.create 20) : ((string, bool) Hashtbl.t))

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
    Xml.Element (tag, [], nodes) -> nodes
  | _ -> raise (No_such_direct_node tag)

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
  | Xml.Element (tag, _, _) -> raise (No_node_content tag)

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
 * @param sentence Xml.xml
 *)
let choose_alt sentence =
  let nr = (List.length sentence) - 1 in
  List.nth sentence (dice nr)

(**
 * Check if alt has a flag attribute and return it
 *
 * @param alt XML
 * @return string list option
 *)
let get_flags alt = 
  let flags = match alt with
      Xml.Element (_, ["setFlag", str], _) ->  Some str
    | Xml.Element (_, x::xs, [Xml.PCData alt_content]) -> raise (Too_many_attributes ("alt: " ^ alt_content))
    | _ -> None
  in
  (* Split flag into list *)
  let flags_list = match flags with
      None -> None
    | Some fs -> Some (Str.split (Str.regexp "[ \t]+") fs)
  in
  flags_list
  
(** 
 * Store flags in flags hash map
 *
 * @param flags_list string list
 *)
let store_flags flags_list = match flags_list with
    None -> ()
  | Some fl ->
    List.iter (fun s ->
      (* Check if flag is already stored. If so, abort *)
      if Hashtbl.mem flags s then 
        raise (Flag_already_set s) 
      else
        Hashtbl.add flags s true
    ) fl

(**
 * Print a sentence with random alt.
 *
 * @param sentence Xml.xml
 *)
let print_sentence sentence =
  let sen = String.trim (fetch_content (sentence)) in
  try (
    let alt = choose_alt (fetch_nodes (sentence) "alt") in
    let flags_list = get_flags alt in
    store_flags flags_list;
    let cont = fetch_content alt in
    sen ^ cont
  )
  with 
    ex -> 
      raise (Sentence_problem (sen, ex))

(**
 * Print all sentences in story
 *
 * @param story XML
 *)
let print_sentences story =
  let sentences = fetch_nodes story "sentence" in
  let string_sentences = List.map (fun s -> (print_sentence s) ^ " ") sentences in
  List.fold_left (^) "" string_sentences

(* Main *)
let _ =

  let filename = if Array.length Sys.argv == 2 then Sys.argv.(1) else raise No_filename in

  Random.self_init ();

  let xml = Xml.parse_file filename in
  let story = fetch_node xml "story" in
  let string_story = try (
    print_sentences story
  )
  with 
    Sentence_problem (sen, ex) ->
      print_endline ("Problem with sentence '" ^ sen ^ "'");
      raise ex
  in
  print_endline string_story
