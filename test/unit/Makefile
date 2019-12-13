include ../../config

QCHECK := $(shell ocamlfind query qcheck-core 2> /dev/null)

ifdef QCHECK

INCLUDES += -I ../../lib fadbad.cma
TESTS = run_test.byte
all: $(TESTS)
run_test.byte: INCLUDES += -I `ocamlfind -query qcheck-core` qcheck_core.cma test_cases.cmo
else
$(error You need to install QuichCheck (run `opam install qcheck`) to run the tests)
endif

run: $(TESTS)
	for test in $(TESTS) ; do ./$$test ; done

clean:
	rm -rf *.cm[ixo] *.byte .depend

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
