(**
 * Use tellstory module and run program
 *
 * @since 2014-03-29
 * @author Olle Härstedt
 *)

open Printf
open ListLabels
open Tellstory

exception No_filename

(* Main *)
let _ =

  (** Create module with dice function module *)
  let module Tellstory = Tellstory.Make(
    struct
      let dice n =
        Random.int n
    end
  ) in

  let filename = if Array.length Sys.argv == 2 then Sys.argv.(1) else raise No_filename in

  Random.self_init ();

  let xml = try Xml.parse_file filename with
    | Xml.Error (msg, pos) ->
        print_endline ("Error while parsing XML file '" ^ filename ^ "'");
        print_int (Xml.line pos);
        print_endline (": " ^ Xml.error_msg msg);
        exit 0;
  in
  let story = Tellstory.fetch_node xml "story" in
  let string_story = try (
    Tellstory.print_sentences story
  )
  with
    | Tellstory.Sentence_problem (sen, Invalid_argument fn) ->
      print_endline (sprintf "Internal error at sentence '%s': Invalid argument: '%s'" sen fn);
      raise (Invalid_argument fn)
    | Tellstory.Sentence_problem (sen, ex) ->
      print_endline ("Problem with sentence '" ^ sen ^ "'");
      (* TODO: Pretty print exceptions *)
      raise ex
  in
  print_endline "";
  print_endline string_story;
  print_endline ""
