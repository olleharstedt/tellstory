(**
 * Unit test cases
 *
 * @since 2014-03-29
 * @author Olle Härstedt
 *)

open OUnit2
open Printf

(** Test sentence tag *)
let test_sentence test_ctxt =

  let module Tellstory = Tellstory.Make(struct
    let dice n = 1
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [Xml.PCData "Test sentence"])
    ]
  ) in
  let story = Tellstory.print_sentences xml in
  assert_equal "Test sentence" story

let tag_test_suite =
  "tags">::: [
    "sentence">:: test_sentence;
  ]

let _ =
  run_test_tt_main tag_test_suite
