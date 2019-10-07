include ../../config.make

INCLUDES += -I ../../lib fadbad.cma

TESTS = test.byte

all: $(TESTS)

test.byte: INCLUDES += -I `ocamlfind -query qcheck-core` qcheck_core.cma test_cases.cmo
test.byte: test.cmo

run: $(TESTS)
	for test in $(TESTS) ; do ./$$test ; done

clean:
	rm -rf *.cm[ixo] *.byte .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
