CMO=lexer.cmo parser.cmo lisptype.cmo exceptions.cmo environments.cmo preparetyper.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=glc
FLAGS=-dtypes

#all: $(BIN)
#	./$(BIN) ./tests/8_funcs.pas
#	gcc -no-pie -g test.s -o a.out
#	./a.out


$(BIN):$(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir --infer -v $<

.mly.mli:
	menhir -v $<

clean:
	rm -f *.cm[io] *.o *.annot *~ $(BIN) $(GENERATED) test.s
	rm -f parser.automaton

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
parser.ml: ast.cmi
preparetyper.ml: wrapast.cmi
