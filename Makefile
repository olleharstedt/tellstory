INCLUDE					:= -package xml-light,str,pcre,core,bolt,ppx_deriving,ppx_deriving.show,netcgi2 -linkpkg -thread -g
PGFLAGS         := --infer
GENERATED       := parser.ml parser.mli lexer.ml
MODULES         := ast globals parser lexer tparser tlexer tellstory main
EXECUTABLE      := tellstory.cgi
OCAMLDEPWRAPPER := ocamldep.wrapper
include Makefile.shared
include Makefile.auto
