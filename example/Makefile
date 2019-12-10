include ../config

FADBAD = ../src/fadbad.cma
INCLUDES += -I ../src/ fadbad.cma

EXAMPLES = \
	exampleFAD.byte \
	exampleBAD.byte \
	exampleFADFAD.byte \
	exampleBADFAD1.byte \
	exampleBADFAD2.byte \
	exampleBADFAD3.byte \
	exampleBADFAD4.byte \
	exampleNewton.byte \
	exampleTAD1.byte \
	exampleTAD2.byte \
	exampleTAD3.byte \
	monteCarlo.byte \
	exampleString.byte \
	exampleBrusselator.byte

all: $(EXAMPLES)

$(EXAMPLES): $(FADBAD)

exampleBrusselator.byte: INCLUDES += graphics.cma unix.cma
brusselator.tmp: exampleBrusselator.byte
	./exampleBrusselator.byte -notad -n 20000 -o brusselator.tmp
brusselator.png: brusselator.tmp
	gnuplot -e "set key autotitle columnhead; set terminal png size 800,800; set output '$@'; plot '$<' using 2:3"

$(FADBAD):
	$(MAKE) -C ../src/

clean:
	rm -rf *.cm[ixo] *.byte .depend
	rm -rf *.tmp *.png
	$(MAKE) -C synchronous clean
	$(MAKE) -C reachability clean

cleanall: clean

-include .depend
.depend:
	ocamldep *.ml *.mli > .depend
