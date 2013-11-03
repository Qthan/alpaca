.PHONY: clean distclean pack count

# OS type: Linux/Win DJGPP
ifdef OS
   EXE=.exe
else
   EXE=
endif

EXEFILE=gracec$(EXE)
MLFILES=Hashcons.ml Identifier.ml Error.ml Types.ml Pretty_print.ml Typeinf.ml Symbol.ml \
   Lexer.ml Ast.ml Quads.ml Intermediate.ml Misc.ml Cfg.ml Local.ml Optimizations.ml \
   Final.ml CodeGen.ml Peephole.ml EmitMasm.ml Parser.ml Main.ml
MLIFILES=Hashcons.mli Identifier.mli Error.mli Types.mli Pretty_print.mli Typeinf.mli Symbol.mli \
   SymbTypes.mli Lexer.mli AstTypes.mli Quads.mli Cfg.mli Local.mli Final.mli Ast.mli Parser.mli

CMOFILES=$(patsubst %.ml,%.cmo,$(MLFILES))
CMIFILES=$(patsubst %.ml,%.cmi,$(MLFILES))
CMXFILES=$(patsubst %.ml,%.cmx,$(MLFILES))
OBJFILES=$(patsubst %.ml,%.o,$(MLFILES))
PARSERFILES=Parser.ml Parser.mli Parser.output Lexer.ml
SRCFILES=Makefile extend.ml Lexer.mll Parser.mly \
  $(filter-out Parser.% Lexer.%,$(MLFILES)) \
  $(filter-out Parser.%,$(MLIFILES))

CAMLP5_FLAGS=-pp "camlp5o ./extend.cmo"
OCAMLC_FLAGS=-g
OCAMLOPT_FLAGS=
OCAMLDOC_FLAGS=-sort -html -colorize-code -d ../doc/
PACKAGES=ocamlgraph
OCAMLC= ocamlfind ocamlc $(OCAMLC_FLAGS) -package $(PACKAGES)
OCAMLOPT=ocamlfind ocamlopt $(OCAMLOPT_FLAGS) -package $(PACKAGES)
OCAMLDEP=ocamlfind ocamldep -package $(PACKAGES)
OCAMLDOC=ocamlfind ocamldoc -package $(PACKAGES)
INCLUDES=

default: Llama 

Llama: $(CMOFILES)
	$(OCAMLC) -o $@ $^ -linkpkg

extend.cmo: extend.ml
	$(OCAMLC) -pp "camlp5o pa_extend.cmo q_MLast.cmo" -I `camlp5 -where` -c $<

%.cmo: %.ml %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmx: %.ml extend.cmo
	$(OCAMLOPT) $(CAMLP5_FLAGS) -c $<

%.cmi: %.mli extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

%.cmo %.cmi: %.ml extend.cmo
	$(OCAMLC) $(CAMLP5_FLAGS) -c $<

.PHONY: all clean count depend

$(EXEFILE): Parser.mli Lexer.ml $(CMOFILES)
	$(OCAMLC) -o $@ $(CMOFILES)

Parser.ml Parser.mli: Parser.mly
	ocamlyacc -v Parser.mly

Lexer.ml: Lexer.mll
	ocamllex Lexer.mll


-include .depend

depend: $(MLFILES) $(MLIFILES) extend.cmo
	$(OCAMLDEP) $(CAMLP5_FLAGS) $(INCLUDES) \
          $(filter-out extend.cmo,$^) > .depend

doc: 
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) $(MLIFILES)

clean:
	$(RM) $(CMXFILES) $(CMOFILES) $(CMIFILES) $(OBJFILES) $(EXEFILES) \
           extend.cmi extend.cmo \
           $(patsubst %,%.cm?,$(EXEFILES)) $(PARSERFILES) pplib.cma *~

distclean: clean
	$(RM) $(EXEFILE) Llama$(EXE) .depend

pack: clean
	tar cvfz gracec.tar.gz $(SRCFILES)

bonus.zip: distclean
	zip bonus.zip README Makefile extend.ml \
	    Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
	    Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml 

bonus.tgz: distclean
	tar cvfz bonus.tgz README Makefile extend.ml \
	    Hashcons.mli Identifier.mli Error.mli Types.mli Symbol.mli \
	    Hashcons.ml Identifier.ml Error.ml Types.ml Symbol.ml 

count:
	wc -l $(SRCFILES)
