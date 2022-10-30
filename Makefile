all: rapport.pdf aritha

rapport.pdf: rapport.tex
	pdflatex rapport.tex

aritha: x86_64.cmo ast.cmo parser.cmo lexer.cmo proj1.cmo
	ocamlc -o aritha x86_64.cmo ast.cmo parser.cmo lexer.cmo proj1.cmo aritha.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll
lexer.cmo: lexer.ml parser.cmo
	ocamlc -c lexer.ml

parser.mli: parser.mly 
	ocamlyacc parser.mly
parser.cmi: parser.mli
	ocamlc -c parser.mli
parser.cmo: parser.cmi
	ocamlc -c parser.ml

ast.cmi: ast.mli
	ocamlc -c ast.mli
ast.cmo: ast.cmi ast.ml
	ocamlc -c ast.ml

proj1.cmi: x86_64.cmo ast.cmo proj1.mli 
	ocamlc -c proj1.mli
proj1.cmo: x86_64.cmo ast.cmo proj1.cmi proj1.ml
	ocamlc -c proj1.ml

x86_64.cmi: x86_64.mli
	ocamlc -c x86_64.mli
x86_64.cmo: x86_64.cmi x86_64.ml
	ocamlc -c x86_64.ml

clean:
	rm -f *.cmi *.cmo *.s parser.mli parser.ml aritha lexer.ml expression.s expression rapport.aux rapport.log