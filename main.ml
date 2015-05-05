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
  Printexc.record_backtrace true;

  (** Create module with dice function module *)
  let module Tellstory = Tellstory.Make(
    struct
      let dice n =
        Random.int n
    end
  ) in

  let filename = if Array.length Sys.argv == 2 then Sys.argv.(1) else raise No_filename in

  Random.self_init ();

  let state = Tellstory.init_state () in

  let string_story = try Tellstory.file_to_string filename state with
  | ex ->
      Printexc.to_string ex
  in
  print_endline "";
  print_endline string_story;
  print_endline ""
