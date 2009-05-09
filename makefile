OUT=socnet.byte

.PHONY: all
all:
	ocamlfind ocamlc -package ocamlgraph socnet.ml -linkpkg -o $(OUT)

.PHONY: clean
clean:
	rm -vf *.cmi *.cmo $(OUT)
