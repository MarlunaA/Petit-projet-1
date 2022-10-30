{
    open Parser
}

rule token = parse 
[' ' '\t']  {token lexbuf} (*on saute tous les espaces et les tabulations*)
|'('        {LBRACE}
|')'        {RBRACE}
|"+."       {ADDF}
|"-."       {SUBF}
|"*."       {MULF}
|'+'        {ADD}
|'-'        {SUB}
|'*'        {MUL}
|'%'        {MOD}
|'/'        {QUOT}
|"int"      {CINT}
|"float"    {CFLOAT}
|['0'-'9']+ as entier    {INT (int_of_string entier)}
|['0'-'9']* '.' ['0'-'9']* as flottant    {FLOAT (float_of_string flottant)}
|'\n'       {EOL}
|eof        {EOL} (*On dit que la fin de l'expression est au retour à la ligne*)