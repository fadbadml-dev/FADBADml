include ../../config

INCLUDES += -I ../../lib fadbad.cma

CMO = interval.cmo affineForm.cmo sets.cmo exampleReachability.cmo

all: objects exampleReachability.byte

objects: $(CMO)

sets.cmo: interval.cmo affineForm.cmo

exampleReachability.byte: INCLUDES += interval.cmo affineForm.cmo sets.cmo

clean:
	rm -rf *.cm[ixo] *.byte .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
