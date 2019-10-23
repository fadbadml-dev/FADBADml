include ../config

CMO = op.cmo fadbad_utils.cmo fadiff.cmo badiff.cmo tadiff.cmo
SORTED_CMO = $(shell ocamldep -sort $(CMO:.cmo=.ml))

all: byte opt

byte: fadbad.cma
opt : fadbad.cmxa

fadbad.cma: $(CMO) fadbad.cmo
	$(OCAMLC) -a -o $@ $(OCAMLFLAGS) $(SORTED_CMO) fadbad.ml

fadbad.cmxa: $(CMO) fadbad.cmo
	$(OCAMLOPT) -a -o $@ $(OCAMLFLAGS) $(SORTED_CMO) fadbad.ml

clean:
	rm -rf *.cm[ox] *.o .depend

cleanall: clean
	rm -rf fadbad.cma fadbad.cmxa fadbad.a *.cmi

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
