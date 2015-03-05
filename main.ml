(*
 *
 * Render info space stations and space ships.
 * 
 * XML for translations.
 * Functor to simulate a "interface" ANIMALTYPE for those parts that
 * are specific for each animal.
 * The target is to create sentences? Or data? Visualized?
 *
 * @since 2014-10-29
 * @author Olle Härstedt <olleharstedt at yahoo dot com>
 *)

open Printf

exception No_node_content of string
exception Not_implemented
exception No_such_lang of string
exception No_such_direct_node of string
exception No_filename

(**
 * The four races
 *)
type race = Rabbit | Bird | Lion | Rat

(**
 * Languages
 *)
type lang = En | Sv

(**
 * Config
 *)
type config = {
  lang : lang;
  race : race;
  xml : Xml.xml
}

(**
 * Sentence type
 * Building blocks of a rendered ship
 *)
type sentence = {
  name : string;
  template : string; 
  variables : string list
}

(**
 * String of lang
 *
 * @param l lang
 * @return string
 *)
let string_of_lang l = match l with
    En -> "en"
  | Sv -> "sv"

(**
 * Lang of string
 *
 * @param s string
 * @return lang
 *)
let lang_of_string s = match s with
    "en" -> En
  | "sv" -> Sv
  | _ -> raise (No_such_lang s)

(**
 * Insert variables into template place-holders
 * A place holder is a %
 *
 * @param sen sentence
 * @return string
 *)
let render_sentence sen = 
  let r = Str.regexp "\\%" in
  let rec replace s l = match l with
      [] -> s
    | x::xs -> replace (Str.replace_first r x s) xs
  in
  replace sen.template sen.variables
;;

(**
 * Race of int
 *
 * @param n int
 * @return race
 *)
let race_of_int n = match n with
    1 -> Rabbit
  | 2 -> Bird
  | 3 -> Lion
  | 4 -> Rat
  | _ -> assert false

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
 * Print a sentence with random alt.
 *)
let print_sentence sentence =
  let sen = fetch_content (sentence) in
  let alt = choose_alt (fetch_nodes (sentence) "alt") in
  let cont = fetch_content alt in
  (String.trim sen) ^ cont

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
  let string_story = print_sentences story in
  print_endline string_story
