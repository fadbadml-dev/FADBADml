include ../../config

INCLUDES += -I ../../lib fadbad.cma

exampleReachability.byte: INCLUDES += interval.cmo affineForm.cmo

clean:
	rm -rf *.cm[ixo] *.byte .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
