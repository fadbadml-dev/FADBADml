CMO = op.cmo utils.cmo fadiff.cmo

all: fadbad.cma

fadbad.cma: $(CMO)
	ocamlc -a -o $@ `ocamldep -sort $(CMO)` $(@:.cma=.ml)

%.cmi %.cmo:%.ml
	ocamlc -c $<

clean:
	rm -rf *.cmo *.cmi

-include .depend
.depend:
	ocamldep $(CMO:.cmo=.ml) > .depend
