(**
 * Unit test cases
 *
 * @since 2014-03-29
 * @author Olle Härstedt
 *)

open OUnit2
open Printf

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
  assert_equal "Test sentence" story

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
  assert_equal "Two alt one" story;

  let module T = Tellstory.Make(struct
    let dice n = 1
  end) in

  let story = T.print_sentences xml in
  assert_equal "Two alt two" story


(** Test suite for all tags *)
let tag_test_suite =
  "tags" >::: [
    "sentence"  >:: test_sentence;
    "alt"       >:: test_alt
  ]

let _ =
  run_test_tt_main tag_test_suite
