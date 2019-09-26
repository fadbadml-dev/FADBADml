include ../../config.make

INCLUDES += -I `ocamlfind -query qcheck-core` qcheck_core.cma \
 			-I ../../src fadbad.cma test.cmo
TESTS = test_fadiff.byte

all: $(TESTS)

clean:
	rm -rf *.cm[ixo] *.byte .depend

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
