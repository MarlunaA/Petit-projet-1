type exp = V 
            | EInt of int | EFloat of float
            |Add of exp * exp |Mul of exp * exp |Sub of exp * exp |Quot of exp * exp |Mod of exp * exp
            |Addf of exp*exp |Mulf of exp*exp |Subf of exp*exp
	          |Cint of exp |Cfloat of exp
	          |Minus of exp |Plus of exp

val bon_type : exp -> int

val parserparla : Lexer.lexeme list -> exp