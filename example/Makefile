include ../config.make

FADBAD = ../src/fadbad.cma
INCLUDE += -I ../src/ fadbad.cma

EXAMPLES = exampleFAD.byte

all: $(EXAMPLES)

$(EXAMPLES): $(FADBAD)

$(FADBAD):
	$(MAKE) -C ../src/

clean:
	rm -rf *.cm[ixo] *.byte .depend

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend