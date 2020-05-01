(**
 * Module for the <alt> tag
 *)

module Make (Common : Common.S) =
  struct
    (** Data types for storing macros *)
    type t = {
      content : string;
      attributes : (string * string) list
    }

  end
