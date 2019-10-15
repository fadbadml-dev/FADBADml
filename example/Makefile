include ../config

FADBAD = ../src/fadbad.cma
INCLUDES += -I ../src/ fadbad.cma

EXAMPLES = \
	exampleFAD.byte \
	exampleFADFAD.byte \
	exampleNewton.byte \
	exampleTAD1.byte \
	exampleTAD2.byte \
	exampleTAD3.byte \
	monteCarlo.byte

all: $(EXAMPLES)

$(EXAMPLES): $(FADBAD)

$(FADBAD):
	$(MAKE) -C ../src/

clean:
	rm -rf *.cm[ixo] *.byte .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
