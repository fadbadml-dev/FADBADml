include ../../../config

INCLUDES += -I ../../../lib fadbad.cma

all: test_interval.byte test_affineForm.byte

test_interval.byte: ../interval.cmo
test_interval.byte: INCLUDES += -I ../ interval.cmo

test_affineForm.byte: ../affineForm.cmo
test_affineForm.byte: INCLUDES += -I ../ affineForm.cmo

clean:
	rm -rf *.cm[ixo] *.byte .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
