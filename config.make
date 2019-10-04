OCAMLC = ocamlc
OCAMLOP = ocamlopt

# implicit rules

.SUFFIXES : .mli .ml .cmi .cmo .cmx .mll .mly .zls .zli .byte .opt

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $(INCLUDES) $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $(INCLUDES) $<

%.cmx %.cmi: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $(INCLUDES:.cma=.cmxa) $<

%.byte: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $(INCLUDES) $<

%.opt: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -o $@ $(INCLUDES:.cma=.cmxa) $<
