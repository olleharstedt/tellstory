INCLUDE					:= -package xml-light,str,pcre,core,bolt,ppx_deriving,ppx_deriving.runtime,ppx_deriving.show,netcgi2 -linkpkg -thread -g
PGFLAGS         := --infer
GENERATED       := bparser.ml bparser.mli blexer.ml
MODULES         := ast globals bparser blexer tparser tlexer tellstory tellstory_cgi
EXECUTABLE      := tellstory.cgi
OCAMLDEPWRAPPER := ocamldep.wrapper
include Makefile.shared
include Makefile.auto
