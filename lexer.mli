type lexeme = 
|LBRACE
|RBRACE
|ADD
|SUB
|MUL
|ADDF
|SUBF
|MULF
|QUOT
|MOD
|Int of int
|Float of float
|INT
|FLOAT
|PLUS
|MINUS

val analyse_lex : string -> lexeme list
