include ../config

FADBAD = ../src/fadbad.cma
INCLUDES += -I ../src/ fadbad.cma

EXAMPLES = \
	exampleFAD.byte \
	exampleBAD.byte \
	exampleFADFAD.byte \
	exampleBADFAD1.byte \
	exampleBADFAD2.byte \
	exampleBADFAD3.byte \
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
