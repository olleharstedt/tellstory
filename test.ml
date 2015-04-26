(**
 * Unit test cases
 *
 * Patching ounit? opam help source, opam help pin
 *
 * @since 2014-03-29
 * @author Olle Härstedt
 *)

open OUnit2
open OUnitTest
open Printf
open ListLabels

exception Test_exception

(** This module is only used for exception handling *)
module T2 = Tellstory.Make(struct
  let dice n = (-1)
end)

(**
 * Create a Tellstory module were dice will return results from dice_results
 * 
 * Example:
 *   make_module [1;2;5] will return a module where the dice call first time 
 *   returns 1, second time 2, then 5.
 *
 * @param dice_results int list
 * @return Tellstory module
 *)
let make_module dice_results : (module Tellstory.T) =
  let dice_calls = ref 0 in (* nr of times dice has been called *)
  (module Tellstory.Make(struct
    let dice n =
      let result = List.nth dice_results (!dice_calls) in
      dice_calls := !dice_calls + 1;
      result
  end))


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
  let story = T.print_sentences xml in

  assert_equal ~msg:"story = One is set" ~printer:id story "One is set";
  assert_equal ~msg:"flag 'one' is set" true ~printer:string_of_bool (Hashtbl.mem Globals.flags_tbl "one")

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
        Xml.Element ("alt", [("setFlag", "one")], []);  (* dice choose 0 here *)
      ]);
      Xml.Element ("sentence", [("ifSet", "one")], [
        Xml.Element ("alt", [("ifSet", "one")], [Xml.PCData "one 1"]);  (* 0 here too *)
        Xml.Element ("alt", [("ifSet", "one")], [Xml.PCData "one 2"]);
        Xml.Element ("alt", [], [Xml.PCData "something else"]);
      ]);
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal ~msg:"" ~printer:id "one 1" story

(** As above *)
let test_if_set_alt2 test_ctxt =

  let dice_results = [0; 1] in  (* first and second dice result *)
  let dice_calls = ref 0 in (* nr of times dice has been called *)
  let module T = Tellstory.Make(struct
    let dice n =
      let result = List.nth dice_results (!dice_calls) in
      dice_calls := !dice_calls + 1;
      result
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [
        Xml.Element ("alt", [("setFlag", "one")], []);  (* dice choose 0 here *)
      ]);
      Xml.Element ("sentence", [("ifSet", "one")], [
        Xml.Element ("alt", [("ifSet", "one")], [Xml.PCData "one 1"]);
        Xml.Element ("alt", [("ifSet", "one")], [Xml.PCData "one 2"]);  (* dice = 1 *)
        Xml.Element ("alt", [], [Xml.PCData "something else"]);
      ]);
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal ~msg:"" ~printer:id "one 2" story

(**
 * <story>
 *   <sentence>
 *     <alt ifSet="one">one 1</alt>
 *   </sentence>
 * </story>
 *)
let test_no_possible_alt test_ctxt =

  let module T = Tellstory.Make(struct
    let dice n = 0
  end) in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("sentence", [], [
        Xml.Element ("alt", [("ifSet", "one")], [Xml.PCData "one 1"]);
      ]);
    ]
  ) in
  assert_raises (T.Sentence_problem ("", "Sentence problem for '': Alt exception 'No possible alts to choose.'")) 
  (fun _ -> 
    T.print_sentences xml
  )

(**
    <macro name="material">
      <alt>stone</alt>
      <alt>wood</alt>
    </macro>

    <sentence>
      This thing is made out of
        <alt useMacro="material"></alt>
    </sentence>

    <sentence>
      This other thing is made of
        <alt useMacro="material"></alt>
    </sentence>
 *)
let test_macro test_ctxt =

  let (module T) = make_module [1;0] in

  let xml = Xml.Element (
    "story", [], [
      Xml.Element ("macro", [("name", "material")], [
        Xml.Element ("alt", [], [Xml.PCData "wood"]);
        Xml.Element ("alt", [], [Xml.PCData "stone"]);
      ]);
      Xml.Element ("sentence", [], [
        Xml.PCData "This thing is made out of";
        Xml.Element ("alt", [("useMacro", "material")], []);
      ]);
      Xml.Element ("sentence", [], [
        Xml.PCData "This other thing is made of";
        Xml.Element ("alt", [("useMacro", "material")], []);
      ]);
    ]
  ) in
  let story = T.print_sentences xml in
  assert_equal ~msg:"" ~printer:id "This thing is made out of stone This other thing is made of wood" story

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
  "if_set_alt", test_if_set_alt;
  "if_set_alt2", test_if_set_alt2;
  "no_possible_alts", test_no_possible_alt;
  "macro", test_macro;
  (* inline alt content test *) (* this should throw an exception: {var error|"space ok in quotation"} <--- space in variable declaration *)
  (* inline macro test *)
  (* variable test *)
  (* record test *)
  (* deck test *)
  (* include test *)
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
  (*
  | T2.Sentence_problem (str, msg, ex) ->
      printf "Sentence problem for '%s': %s" str (T2.string_of_exn ex)
  *)
