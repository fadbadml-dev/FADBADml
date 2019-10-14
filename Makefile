include config

all: lib_byte lib_opt

byte: lib_byte
opt : lib_opt

config:
	./configure

install: config
	mkdir -p $(LIBDIR)
	@echo libdir: $(LIBDIR)
	cp lib/* $(LIBDIR)

lib: config
	$(MAKE) -C src all
	mkdir -p lib
	cp src/*.cmi src/*.cma src/*.cmxa src/*.a lib

lib_byte: config
	$(MAKE) -C src byte
	mkdir -p lib
	cp src/*.cmi src/*.cma lib

lib_opt: config
	$(MAKE) -C src opt
	mkdir -p lib
	cp src/*.cmi src/*.cmxa src/*.a lib

doc: lib
	mkdir -p doc/html
	ocamldoc -html -d doc/html -I src src/*.ml

example: lib_byte
	$(MAKE) -C example

test: lib_byte
	$(MAKE) -C test

run_test: test
	$(MAKE) -C test run

clean:
	$(MAKE) -C src clean
	$(MAKE) -C example clean
	$(MAKE) -C test clean

cleanall realclean mrproper: clean
	$(MAKE) -C src cleanall
	$(MAKE) -C example cleanall
	$(MAKE) -C test cleanall
	rm -rf lib doc
	rm -f config META opam

.PHONY: lib doc example test
