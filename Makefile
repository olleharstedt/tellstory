# Add --table on the next line to use Menhir's table-based back-end.
INCLUDE					:= -package xml-light,str,pcre -linkpkg tellstory.mli
PGFLAGS         := --infer
GENERATED       := parser.ml parser.mli lexer.ml
MODULES         := globals parser lexer tellstory
EXECUTABLE      := tellstory
OCAMLDEPWRAPPER := ocamldep.wrapper
include Makefile.shared
include Makefile.auto
