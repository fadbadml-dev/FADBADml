CMO = op.cmo utils.cmo fadiff.cmo
SORTED_CMO = $(shell ocamldep -sort $(CMO:.cmo=.ml))

all: fadbad.cma

fadbad.cma: $(CMO) fadbad.cmo
	ocamlc -a -o $@ $(SORTED_CMO) fadbad.ml

%.cmi:%.mli
	ocamlc $<
%.cmo:%.ml
	ocamlc -c $<

clean:
	rm -rf *.cmo *.cmi

-include .depend
.depend:
	ocamldep $(CMO:.cmo=.ml) > .depend
