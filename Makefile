INCLUDE					:= -package xml-light,str,pcre,core -linkpkg -thread
PGFLAGS         := --infer
GENERATED       := parser.ml parser.mli lexer.ml
MODULES         := globals parser lexer tellstory main
EXECUTABLE      := tellstory
OCAMLDEPWRAPPER := ocamldep.wrapper
include Makefile.shared
include Makefile.auto
