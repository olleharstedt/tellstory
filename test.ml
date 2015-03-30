(**
 * Unit test cases
 *
 * @since 2014-03-29
 * @author Olle Härstedt
 *)

open OUnit2
open Printf
open ListLabels

(** Identiy function for strings *)
let id x = sprintf "'%s'" x

(**
 * Test sentence tag
 *
 * <story>
 *   <sentence>Test sentence</sentence>
 * </story>
 *)
let test_sentence test_ctxt =

  let module T = Tellstory.Make(struct
    let dice n = 1
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [Xml.PCData "Test sentence"])
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal ~msg:"sentence" ~printer:id "Test sentence" story

(**
 * Test alt tag
 *
 * <story>
 *  Two alt
 *    <alt>one</alt>
 *    <alt>two</alt>
 * </story>
 *)
let test_alt test_ctxt =

  let module T = Tellstory.Make(struct
    let dice n = 0
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [
        Xml.PCData "Two alt";
        Xml.Element ("alt", [], [Xml.PCData "one"]);
        Xml.Element ("alt", [], [Xml.PCData "two"]);
      ])
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal ~msg:"alt 1" ~printer:id "Two alt one" story;

  let module T = Tellstory.Make(struct
    let dice n = 1
  end) in

  let story = T.print_sentences xml in
  assert_equal ~msg:"alt 2" ~printer:id "Two alt two" story

(**
 * <story>
 *   <alt>one</alt>
 *   <alt>two</alt>
 * </story>
 *)
let test_alt_with_empty_sentence test_ctxt =
  let module T = Tellstory.Make(struct
    let dice n = 0
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [
        Xml.Element ("alt", [], [Xml.PCData "one"]);
        Xml.Element ("alt", [], [Xml.PCData "two"]);
      ])
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal ~msg:"alt empty 1" ~printer:id "one" story;

  let module T = Tellstory.Make(struct
    let dice n = 1
  end) in

  let story = T.print_sentences xml in
  assert_equal ~msg:"alt empty 2" ~printer:id "two" story

(**
 * <story>
 *   <sentence>
 *     <alt setFlag="one">one</alt>
 *   </sentence>
 * </story>
 *)
let test_set_flag test_ctxt =
  let module T = Tellstory.Make(struct
    let dice n = 0
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [
        Xml.Element ("alt", [("setFlag", "one")], [Xml.PCData "one"]);
      ])
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal ~msg:"'one' is member of hashtbl" ~printer:string_of_bool true (Hashtbl.mem Globals.flags_tbl "one");
  assert_equal ~msg:"'nothing' is not member of hashtbl" ~printer:string_of_bool false (Hashtbl.mem Globals.flags_tbl "nothing");
  assert_equal ~msg:"'one' is printed" ~printer:id "one" story

(**
 * <story>
 *   <sentence>
 *     <alt setFlag="one"></alt>
 *   </sentence>
 *   <sentence ifSet="one">
 *     One is set
 *   </sentence>
 * </story>
 *)
let test_if_set test_ctxt =

  let module T = Tellstory.Make(struct
    let dice n = 0
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [
        Xml.Element ("alt", [("setFlag", "one")], []);
      ]);
      Xml.Element ("sentence", [("ifSet", "one")], [Xml.PCData "One is set"]);
    ]
  ) in
  let story = try T.print_sentences xml with
    | T.Sentence_problem (str, ex) -> printf "%s" (T.string_of_exn ex); raise ex
  in

  ()
  (*
  printf "'%s'" story;
  assert_equal story story;
  assert_equal true (Hashtbl.mem Globals.flags_tbl "one")
  *)
  (*
  *)

(**
 * <story>
 *   <sentence>
 *     <alt setFlag="one"></alt>
 *   </sentence>
 *   <sentence>
 *     <alt ifSet="one">one 1</alt>
 *     <alt ifSet="one">one 2</alt>
 *     <alt>something else</alt>
 *   </sentence>
 * </story>
 *)
let test_if_set_alt test_ctxt =

  let module T = Tellstory.Make(struct
    let dice n = 0
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [
        Xml.Element ("alt", [("setFlag", "one")], []);
      ]);
      Xml.Element ("sentence", [("ifSet", "one")], [
        Xml.Element ("alt", [("ifSet", "one")], [Xml.PCData "one 1"]);
        Xml.Element ("alt", [("ifSet", "one")], [Xml.PCData "one 2"]);
        Xml.Element ("alt", [], [Xml.PCData "something else"]);
      ]);
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal "one 1" story;

  let module T = Tellstory.Make(struct
    let dice n = 1
  end) in
  let story = T.print_sentences xml in
  assert_equal "one 2" story;

  let module T = Tellstory.Make(struct
    let dice n = 1
  end) in
  let story = T.print_sentences xml in
  assert_equal "one 2" story

(**
 * Tear-down
 * Have to unset flags in flags_tbl hashtable after each test
 *
 * @param () ?
 * @param test_ctxt
 * @return unit
 *)
let tear_down () test_ctxt =
  Hashtbl.clear Globals.flags_tbl

(**
 * List of test cases as (name, function) tuples
 *)
let test_list = [
  "sentence", test_sentence;
  "alt", test_alt;
  "alt_empty_sentence", test_alt_with_empty_sentence;
  "set_flag", test_set_flag;
  "if_set", test_if_set;
]

(** Test suite for all tags *)
let tag_test_suite =
  "tags" >::: (map test_list ~f:(fun (name, test_fn) ->
    name >:: (fun test_ctxt ->
      bracket ignore tear_down test_ctxt;
      test_fn test_ctxt
    )
  ))

let _ =
  run_test_tt_main tag_test_suite
