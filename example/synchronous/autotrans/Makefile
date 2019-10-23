include ../../../config

.PHONY: test debug

ZELUC = zeluc -copy
OCAMLC = ocamlc -g
OCAMLFLAGS += -I `zeluc -where`
ZLSTDLIBS = bigarray.cma unix.cma -I `ocamlfind query sundialsml` sundials.cma
ZLGTKLIBS = $(ZLSTDLIBS) -I `ocamlfind query lablgtk2` lablgtk.cma

INCLUDES = $(ZLGTKLIBS) zllibgtk.cma

INCLUDES += -I ../../../lib fadbad.cma

OBJ=autotranc_main.cmo autotransd_main.cmo \
		autotransc.cmo autotransd.cmo \
	  common.cmo consts.cmo draw.cmo maneuvers.cmo utils.cmo

DEPS=maneuvers.cmo utils.cmo consts.cmo common.cmo

EXE=autotransd.byte

all: byte
byte: $(EXE)

# ------------------------------------

CDEPS = maneuvers.cmo utils.cmo consts.cmo common.cmo \
    		autotransc.cmo autotransc_main.cmo

autotransc.byte: ZLEXTRALIBS = $(ZLGTKLIBS)
autotransc.byte: INCLUDES += $(DEPS)
autotransc.byte: $(DEPS) autotransc.ml autotransc_main.ml

autotransc.ml autotransc_main.ml: autotransc.zls common.zci maneuvers.zci
	$(ZELUC) -gtk2 -o autotransc_main -s main $<

autotransc.byte:autotransc.ml autotransc_main.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) $(@:.byte=.ml) $(@:.byte=_main.ml)

autotransc.zci: maneuvers.zci

# ------------------------------------

autotransd.byte autotransd.cmo: ZLEXTRALIBS = $(ZLGTKLIBS)
autotransd.byte autotransd.cmo: INCLUDES += myOp.cmo $(DEPS)
autotransd.byte autotransd.cmo: $(DEPS) autotransd.ml autotransd_main.ml

autotransd.ml autotransd_main.ml: autotransd.zls common.zci maneuvers.zci
	$(ZELUC) -gtk2 -o autotransd_main -s main $<

autotransd.byte:autotransd.ml autotransd_main.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) $(@:.byte=.ml) $(@:.byte=_main.ml)
# ------------------------------------

main_fadbad.byte: INCLUDES += myOp.cmo $(DEPS)
main_fadbad.byte: autotransd.cmo main_fadbad.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) $^

# ------------------------------------

common.zci: consts.zci utils.zci
utils.zci: myOp.zci

maneuvers.cmo: myOp.cmo

# ------------------------------------

# test:
# 	$(MAKE) -C spec test
# run_tests:
# 	$(MAKE) -C spec run_tests

# ------------------------------------

%.zci:%.zli
	$(ZELUC) $<
%.zci:%.zls
	$(ZELUC) $<


# ------------------------------------

clean:
	-@rm -f autotransc_main.ml autotransc.ml
	-@rm -f autotrans_gui_main.ml autotrans_gui.ml
	-@rm -f autotransd_main.ml autotransd.ml
	-@rm -f common.ml
	-@rm -f *.o *.cm[oix] *.annot *.obc *.zci
	# $(MAKE) -C spec clean

realclean cleanall: clean
	-@rm -f autotransc.byte
	-@rm -f autotrans_gui.byte
	-@rm -f autotransd.byte
	-@rm -f main_fadbad.byte
	# $(MAKE) -C spec cleanall

.depend: depend
depend: $(OBJ:.cmo=.ml)
	@$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

-include .depend
