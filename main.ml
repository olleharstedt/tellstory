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
  let module Tellstory = Tellstory.Make
    (struct
      let dice n = Random.int n
    end)
    (struct
      let print_string = print_string
      let print_endline = print_endline
      let input_line = input_line
      let flush = flush_all
    end)
  in

  (*
  let inter = Unix.getenv "INTERACTIVE" in
  print_endline inter;
  *)

  let filename = if Array.length Sys.argv == 2 then Sys.argv.(1) else raise No_filename in

  Random.self_init ();

  let state = Tellstory.init_state () in

  let _ = try Tellstory.file_to_string filename state with
    | ex -> Printexc.to_string ex
  in
  ()
  (*printf "%s\n" string_story;*)

(** tmp for template_lang *)
(*
let () =
  let stdinbuf = Lexing.from_channel stdin in
  while true do
    (* Read line by line. *)
    let linebuf = Lexing.from_string (Tlexer.line stdinbuf) in
    try
      (* Run the parser on a single line of input. *)
      let ast = (Tparser.main Tlexer.token linebuf) in
      let ast_string = Ast.show_nameterm_list ast in
      Printf.printf "%s\n%!" ast_string;
    with
    | Tlexer.Error msg ->
        Printf.fprintf stderr "%s%!" msg
    | Tparser.Error ->
        Printf.fprintf stderr "At offset %d: syntax error.\n%!" (Lexing.lexeme_start linebuf)
  done
*)
