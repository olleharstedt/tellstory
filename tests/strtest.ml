module type FOO =
  sig
    type s
    val compare : s -> s -> int
  end

let _ =
  let s = (module String : FOO) in
  ()
