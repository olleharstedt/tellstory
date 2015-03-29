(**
 * Global variables accessed in both lexer and tellstory.
 *
 * @since 2015-03-26
 * @author Olle Härstedt
 *)

(* Hash table to store flags, with flag name as key. Flags are used for branching. *)
let flags_tbl = ((Hashtbl.create 20) : ((string, bool) Hashtbl.t))
