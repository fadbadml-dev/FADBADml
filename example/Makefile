include ../config.make

FADBAD = ../src/fadbad.cma
INCLUDES += -I ../src/ fadbad.cma

EXAMPLES = exampleFAD.byte exampleFADFAD.byte

all: $(EXAMPLES)

$(EXAMPLES): $(FADBAD)

$(FADBAD):
	$(MAKE) -C ../src/

clean:
	rm -rf *.cm[ixo] *.byte .depend

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
