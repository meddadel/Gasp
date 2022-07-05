run.exe: run.native
	mv run.native run.exe
run.native: parser.mly interpreter.ml lexer.mll run.ml syntax.ml
	ocamlbuild run.native -libs graphics -use-menhir
clean:
	rm -r _build
	rm run.exe
