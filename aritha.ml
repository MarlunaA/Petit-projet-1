let usage = "Usage : ./aritha expression.exp"

let compile_exp exp file =
  let arbre = Parser.parse Lexer.token exp in
  Proj1.compile file arbre

let _ =
  if Array.length Sys.argv <= 1 then print_endline usage
  else let exp_file = Sys.argv.(1) in
    let ic = open_in exp_file in
    let line = Lexing.from_channel ic in
    let file = (String.sub exp_file 0 (String.index exp_file '.'))^".s" in
    compile_exp line file;
    flush stdout;
    close_in ic