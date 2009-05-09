OUT=socnet.byte

.PHONY: all
all: externals $(OUT)

$(OUT) : socnet.ml
	ocamlfind ocamlc -package ocamlgraph socnet.ml -linkpkg -o $(OUT)

.PHONY: externals
externals: externals/Ubigraph.cmi externals/Ubigraph.cmo

externals/Ubigraph.cmo : externals/Ubigraph.ml externals/Ubigraph.cmi
	ocamlfind ocamlc -package xmlrpc-light -I ./externals externals/Ubigraph.ml -c

externals/Ubigraph.cmi : externals/Ubigraph.mli 
	ocamlfind ocamlc -package xmlrpc-light externals/Ubigraph.mli -c


.PHONY: clean
clean:
	rm -vf *.cmi *.cmo $(OUT)
	rm -vf externals/*.cmi externals/*.cmo

