INCLUDE			:= -package xml-light,str,pcre,ppx_deriving,ppx_deriving.runtime,ppx_deriving.show -linkpkg -thread -g
PGFLAGS         := --infer
GENERATED       := bparser.ml bparser.mli blexer.ml
# Change "main" to tellstory_cgi to compile CGI script.
MODULES         := ast globals bparser blexer tparser tlexer tellstory main
EXECUTABLE      := tellstory
OCAMLDEPWRAPPER := ocamldep.wrapper
include Makefile.shared
include Makefile.auto
