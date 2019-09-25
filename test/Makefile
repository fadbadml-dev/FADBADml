FADBAD = ../src/fadbad.cma

OCAMLINCLUDE = -I ../src/

all: exampleFAD.byte

%.byte: $(FADBAD) %.cmo
	ocamlc $(OCAMLINCLUDE) -o $@ $(FADBAD) $(@:.byte=.cmo)

$(FADBAD):
	$(MAKE) -C ../src/

%.cmi %.cmo: %.ml
	ocamlc $(OCAMLINCLUDE) -c $<

clean:
	rm -rf *.cmo *.cmi *.byte

-include .depend
.depend:
	ocamldep $(CMO:.cmo=.ml) > .depend
