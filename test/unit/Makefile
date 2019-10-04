include ../../config.make

INCLUDES += -I ../../src fadbad.cma

TESTS = test_fadiff.byte test_badiff.byte

all: $(TESTS)

test_fadiff.byte: INCLUDES += -I `ocamlfind -query qcheck-core` qcheck_core.cma \
 			test.cmo

run: $(TESTS)
	for test in $(TESTS) ; do ./$$test ; done

clean:
	rm -rf *.cm[ixo] *.byte .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
