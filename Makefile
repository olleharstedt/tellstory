#a.out: spacestation.ml
	#ocamlfind ocamlopt -package xml-light,str,ppx_deriving.show -linkpkg spacestation.ml

tellstory: main.ml
	ocamlfind ocamlopt -g -package xml-light,str,pcre -linkpkg main.ml -o tellstory

all: a.out
