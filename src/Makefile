CMO=op.cmo utils.cmo fadiff.cmo

all: $(CMO)

%.cmi %.cmo:%.ml
	ocamlc -c $^

clean:
	rm -rf *.cmo *.cmi

-include .depend
.depend:
	ocamldep $(CMO:.cmo=.ml) > .depend
