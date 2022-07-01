CMO=lexer.cmo ast.cmo parser.cmo lisptype.cmo utils.cmo exceptions.cmo environments.cmo wrapast.cmo preparetyper.cmo x86_64.cmo codegeneration.ml gcccalls.cmo main.cmo
GENERATED=lexer.ml parser.ml parser.mli
BIN=glc
FLAGS=-dtypes unix.cma
RUNTIMEPATH=./c_runtime_and_stdlib

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

runtime:
	gcc -o $(RUNTIMEPATH)/compiled/runtime.s -S -fverbose-asm $(RUNTIMEPATH)/src/runtime.c 

include .depend
parser.ml: ast.cmo
preparetyper.ml: wrapast.cmo
