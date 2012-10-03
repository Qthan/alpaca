OCAMLC_FLAGS=-g
OCAMLC=ocamlc
OCAMLDEP=ocamldep

%.cmo: %.ml %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmi: %.mli
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

%.cmo %.cmi: %.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

Llama: Ast.cmo Lexer.cmo Parser.cmo Main.cmo
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $^

Lexer.ml: Lexer.mll
	ocamllex -o $@ $<

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

Ast.mli: Ast.ml
	$(OCAMLC) $(OCAMLC_FLAGS) -c $<

.PHONY: clean distclean

-include .depend

depend: Ast.mli Ast.ml Lexer.ml Lexer.mli Parser.ml Parser.mli Main.ml
	$(OCAMLDEP) $^ > .depend

clean:
	$(RM) Ast.mli Lexer.ml Parser.ml Parser.mli Parser.output *.cmo *.cmi *~

distclean: clean
	$(RM) Llama .depend



# DO NOT DELETE
