all:
	@ocamlc -c procedure.ml
	@clear
	@ocamllex lexer.mll       # generates lexer.ml
	@ocamlyacc parser.mly     # generates parser.ml and parser.mli
	@ocamlc -c parser.mli
	@ocamlc -c lexer.ml
	@ocamlc -c parser.ml
	@ocamlc -c tester.ml
	@ocamlc -o test procedure.cmo lexer.cmo parser.cmo tester.cmo
	@clear
clean:
	rm test *.cmo *.cmi *.mli lexer.ml parser.ml
