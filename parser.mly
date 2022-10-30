
    %token ADD SUB MUL ADDF SUBF MULF QUOT MOD 
    %token CINT CFLOAT
    %token <int> INT
    %token <float> FLOAT 
    %token LBRACE RBRACE EOL

    %left ADD SUB  ADDF SUBF 
    %left MUL MULF
    %left QUOT MOD

    %nonassoc CINT CFLOAT 

%start parse 
    %type <Ast.exp> parse
    %%
    parse:
        |expr EOL     {$1}
        |expr         {$1}
    expr:
        |INT    {Ast.EInt($1)}
        |FLOAT      {Ast.EFloat($1)}
        |expr ADD expr   {Ast.Add($1, $3)}
        |expr SUB expr   {Ast.Sub($1, $3)}
        |expr MUL expr   {Ast.Mul($1, $3)}
        |expr ADDF expr   {Ast.Addf($1, $3)}
        |expr SUBF expr   {Ast.Subf($1, $3)}
        |expr MULF expr   {Ast.Mulf($1, $3)}
        |expr QUOT expr   {Ast.Quot($1, $3)}
        |expr MOD expr   {Ast.Mod($1, $3)}
        |CINT expr     {Ast.Cint($2)}
        |CFLOAT expr    {Ast.Cfloat($2)}
        |SUB expr   {Ast.Minus($2)}
        |ADD expr   {$2}
        |LBRACE expr RBRACE     {$2}

