include config

all: lib

byte: lib_byte
opt : lib_opt

config META:
	./configure

install: lib META
	mkdir -p $(LIBDIR)
	cp lib/* $(LIBDIR)
	$(OCAMLFIND) install fadbadml META

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
	mkdir -p doc
	ocamldoc $(OCAMLFLAGS) -html -d doc -css-style ../css/doc_style.css -verbose \
		-hide Stdlib,Fadiff,Badiff,Tadiff,Fadbad_utils \
		-t FADBADml -show-missed-crossref -charset utf8 -short-functors \
		-short-paths \
		-I src \
		$(shell find src -name "*.mli") \
		$(shell find src -name "*.ml" -a ! -name 'fadbad_utils.ml')

makedist:
	mkdir -p fadbadml
	cp -r example fadbadml
	cp -r lib fadbadml

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
	rm -f config META

.PHONY: lib doc example test
