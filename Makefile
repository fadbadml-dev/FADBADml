all: lib

lib:
	$(MAKE) -C src
	mkdir -p lib
	cp src/*.cmi src/fadbad.cma lib

doc: lib
	mkdir -p doc/html
	ocamldoc -html -d doc/html -I src src/*.ml

test: lib
	$(MAKE) -C test

run_test: test
	$(MAKE) -C test run

example: lib
	$(MAKE) -C example

clean:
	$(MAKE) -C src clean
	$(MAKE) -C example clean
	$(MAKE) -C test clean

cleanall: clean
	$(MAKE) -C src cleanall
	$(MAKE) -C example cleanall
	$(MAKE) -C test cleanall
	rm -rf doc lib

.PHONY: lib doc example test
