.PHONY: clean

artify: artify.ml
	ocamlfind ocamlopt -linkpkg -package camomile -package str -o $@ -g $<
	rm -f $@.cmi $@.cmx $@.o

clean:
	rm -f artify *.cmi *.cmx
