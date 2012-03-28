OCAMLC_FLAGS=-g
OCAMLC=ocamlc

%.cmo: %.ml %.mli
		$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
		$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
		$(OCAMLC) $(OCAMLC_FLAGS) -c $<

Llexer: mylexer.cmo
		$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

mylexer.ml: mylexer.mll
		ocamllex -o $@ $<

.PHONY: clean distclean

clean:
		$(RM) mylexer.ml *.cmo *.cmi *~

distclean: clean
		$(RM) Llexer$(EXE)
