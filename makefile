OUT=socnet.byte

.PHONY: all
all: externals $(OUT)

$(OUT) : socnet.cmo externals/Ubigraph.cmi externals/Ubigraph.cmo
	ocamlfind ocamlc \
	  -package ocamlgraph,xmlrpc-light \
	  externals/Ubigraph.cmo \
	  socnet.cmo \
	  -linkpkg -o $(OUT)

socnet.cmo : socnet.ml
	ocamlfind ocamlc -package ocamlgraph -I ./externals socnet.ml -c

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

