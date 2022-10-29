open Lexer

let usage = "Usage : ./aritha expression.exp"

let compile_exp exp file =
  let liste_lex = Lexer.analyse_lex exp in
  let arbre = Parserparla.parserparla liste_lex in
  Proj1.compile file arbre

let _ =
  if Array.length Sys.argv <= 1 then print_endline usage
  else let exp_file = Sys.argv.(1) in
    let ic = open_in exp_file in
    let line = input_line ic in
    let file = (String.sub exp_file 0 (String.index exp_file '.'))^".s" in
    print_endline line;
    compile_exp line file;
    flush stdout;
    close_in ic