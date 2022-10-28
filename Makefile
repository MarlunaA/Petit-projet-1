aritha: x86_64.cmo proj1.cmo lexer.cmo parserparla.cmo
	ocamlc -o aritha x86_64.cmo lexer.cmo parserparla.cmo proj1.cmo aritha.ml

proj1.cmi: x86_64.cmo parserparla.cmo proj1.mli
	ocamlc -c proj1.mli
proj1.cmo: x86_64.cmo proj1.cmi proj1.ml
	ocamlc -c proj1.ml

parserparla.cmi: lexer.cmo parserparla.mli
	ocamlc -c parserparla.mli
parserparla.cmo: parserparla.cmi parserparla.ml
	ocamlc -c parserparla.ml

lexer.cmi: lexer.mli
	ocamlc -c lexer.mli
lexer.cmo: lexer.cmi lexer.ml
	ocamlc -c lexer.ml

x86_64.cmi: x86_64.mli
	ocamlc -c x86_64.mli
x86_64.cmo: x86_64.cmi x86_64.ml
	ocamlc -c x86_64.ml

clean:
	rm -f *.s *.cmi *.cmo
