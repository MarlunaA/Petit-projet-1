type exp = V | EInt of int | EFloat of float
           |Add of exp * exp |Mul of exp * exp |Sub of exp * exp |Quot of exp * exp |Mod of exp * exp
           |Addf of exp*exp |Mulf of exp*exp |Subf of exp*exp
	   |Cint of exp |Cfloat of exp
	   |Minus of exp |Plus of exp
	   

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
|MINUS;;

(*les recherche_pack cherchent dans la suite de la liste l'expression � laquelle va s'appliquer
  respectivement la conversion (int->float ou l'inverse) ou le modulo,
  et renvoie le couple de cet argument avec le reste de la liste*)

let recherche_pack_conv u =
  let rec cherche u compteur v =
    match u with
      |[] -> failwith"pas bon parenth�sage"
      |LBRACE :: xs ->
	if compteur  = 0 then failwith"probl�me �a ne devrait m�me pas arriver"
	else cherche xs (compteur + 1) (LBRACE :: v)
      |RBRACE :: xs ->
	if (compteur = 1) then (List.rev(v), xs)
	else cherche xs (compteur - 1) (RBRACE :: v)
      |x :: xs ->
	cherche xs compteur (x :: v)
  in
  match u with
    |[] -> failwith"conversion de rien"
    |x :: xs ->
      if x <> LBRACE then failwith"une conversion est commenc�e par une ("
      else cherche xs 1 []

let recherche_pack_mod u =
  let rec cherche u compteur v =
    match u with
      |[] -> failwith"pas bon parenth�sage"
      |LBRACE :: xs ->
	if compteur  = 0 then failwith"probl�me �a ne devrait m�me pas arriver"
	else cherche xs (compteur + 1) (LBRACE :: v)
      |RBRACE :: xs ->
	if (compteur = 1) then (List.rev(v), xs)
	else cherche xs (compteur - 1) (RBRACE :: v)
      |x :: xs ->
	cherche xs compteur (x :: v)
  in
  match u with
    |[] -> failwith"mudulo de rien"
    |Int(x) :: xs -> ([Int(x)], xs)
    |x :: xs ->
      if x <> LBRACE then failwith"un modulo est commenc� par des parenth�ses ou s'applique sur un seul �l�ment entier"
      else cherche xs 1 []

let recherche_pack_unaire u =
  let rec cherche u compteur v =
    match u with
      |[] -> failwith"pas bon parenth�sage"
      |LBRACE :: xs ->
	if compteur  = 0 then failwith"probl�me �a ne devrait m�me pas arriver"
	else cherche xs (compteur + 1) (LBRACE :: v)
      |RBRACE :: xs ->
	if (compteur = 1) then (List.rev(v), xs)
	else cherche xs (compteur - 1) (RBRACE :: v)
      |x :: xs ->
	cherche xs compteur (x :: v)
  in
  match u with
    |[] -> failwith"unaire de rien"
    |x :: xs ->
      if x <> LBRACE then failwith"une op�ration unaire commence par des parenth�ses"
      else cherche xs 1 []




(*reparcourt l'arbre final pour v�rifier le typage des entiers/flottants*)

let bon_type exp = 
  let rec typ exp =
    match exp with
      |V -> 0
      |Add(e1, e2) ->
	if ((typ e1) <> 0) or ((typ e2) <> 0) then 2
	else 0
      |Addf(e1, e2) ->
	if ((typ e1) <> 1) or ((typ e2) <> 1) then 2
	else 1
      |Mul(e1, e2) ->
	if (typ e1 <> 0) or (typ e2 <> 0) then 2
	else 0
      |Mulf(e1, e2) ->
	if (typ e1 <> 1) or (typ e2 <> 1) then 2
	else 1
      |Sub(e1, e2) ->
	if (typ e1 <> 0) or (typ e2 <> 0) then 2
	else 0
      |Subf(e1, e2) ->
	if (typ e1 <> 1) or (typ e2 <> 1) then 2
	else 1
      |Cfloat(e1) ->
	if (typ e1 <> 0) then 2
	else 1
      |Cint(e1) ->
	if (typ e1 <> 1) then 2
	else 0
      |Quot(e1, e2) ->
	if (typ e1 <> 0) or (typ e2 <> 0) then 2
	else 0
      |Mod(e1, e2) ->
	if (typ e1 <> 0) or (typ e2 <> 0) then 2
	else 0
      |EInt(x) -> 0
      |EFloat(x) -> 1
  in
  let b = typ exp in
  if b = 2 then false
  else true
;;



(*fonction principale*)

let parserparla u =
  let rec parcourt u gauche = (*gauche est l'arbre gauche de la prochaine op�ration
			      renvoie un arbre (type exp)*)
    let rec parcourt_droite v compteur vus = (*v est le reste de la liste de lexemes,
					       compteur le compteur de parenth�ses,
					       vus la liste des lexemes d�j� vus
					       (dont l'AST correspondant deviendra le membre droit de l'arbre dans l'appel qui a appel� parcourt_droite*)
      (*renvoie (expression, liste de lexemes)*)
      match v with
	|[] -> (parcourt (List.rev vus) V, [])
	|Int(x) :: xs -> parcourt_droite xs compteur (Int(x) :: vus) 
	|Float(x) :: xs -> parcourt_droite xs compteur (Float(x) :: vus) 
	|LBRACE :: xs ->
	  if vus = [] then parcourt_droite xs 1 []
	  else parcourt_droite xs (compteur + 1) (LBRACE :: vus)
	|RBRACE :: xs ->
	  parcourt_droite xs (compteur - 1) (RBRACE :: vus)
	|INT :: xs ->
	  parcourt_droite xs compteur (INT :: vus)
	|FLOAT :: xs ->
	  parcourt_droite xs compteur (FLOAT :: vus)
	|MOD :: xs ->
	  parcourt_droite xs compteur (MUL :: vus)
	|PLUS :: xs ->
	  parcourt_droite xs compteur (PLUS :: vus)
	|MINUS :: xs ->
	  parcourt_droite xs compteur (MINUS :: vus)
	|MUL :: xs ->
	  parcourt_droite xs compteur (MUL :: vus)
	|MULF :: xs ->
	  parcourt_droite xs compteur (MULF :: vus)
	|x :: xs ->
	  if compteur = 0 then (parcourt (List.rev vus) V, v)
	  else parcourt_droite xs compteur (x :: vus)
    in
    (*fonction raccourci utilis�e dans parcourt quand on arrive sur une op�ration*)
    
    let gen xs gauche op =
      if gauche = V then failwith"pas d'argument � gauche"
      else if xs = [] then failwith"pas d'argument � droite"
      else
	let (droit, reste) = parcourt_droite xs 0 [] in (*droit est une exp, reste est xs � laquelle on a tronqu� les �l�ments pris pour faire droit*)
	let g = op (gauche,droit) in (*g est une exp*)
	parcourt reste g
    in
    
    match u with
      |[] -> gauche
      |LBRACE :: xs ->
	if xs = [] then failwith"finit pas une parenth�se ouvrante"
	else let (droit, reste) = parcourt_droite u 0 [] in
	     parcourt reste droit
      |RBRACE :: xs -> gauche;
      |MUL :: xs -> gen xs gauche (function (x, y) -> Mul (x, y))
      |MULF :: xs -> gen xs gauche (function (x, y) -> Mulf (x, y))
      |ADD :: xs -> gen xs gauche (function (x, y) -> Add (x, y))
      |ADDF :: xs -> gen xs gauche (function (x, y) -> Addf (x, y))
      |SUB :: xs -> gen xs gauche (function (x, y) -> Sub (x, y))
      |SUBF :: xs -> gen xs gauche (function (x, y) -> Subf (x, y))
      |QUOT :: xs -> 
	let (pack, reste) = recherche_pack_mod xs in
	let g = Quot(gauche, parcourt pack V) in
	parcourt reste g
      |MOD :: xs ->
	let (pack, reste) = recherche_pack_mod xs in
	let g = Mod(gauche, parcourt pack V) in
	parcourt reste g
      |INT :: xs ->
	let (pack, reste) = recherche_pack_conv xs in
	let g = Cint(parcourt pack V) in
	parcourt reste g
      |FLOAT :: xs -> 
	let (pack, reste) = recherche_pack_conv xs in
	let g = Cfloat(parcourt pack V) in
	parcourt reste g
      |PLUS :: xs ->
	let (pack, reste) = recherche_pack_unaire xs in
	let g = Plus(parcourt pack V) in
	parcourt reste g
      |MINUS :: xs ->
	let (pack, reste) = recherche_pack_unaire xs in
	let g = Minus(parcourt pack V) in
	parcourt reste g
      |Int(x) :: xs -> parcourt xs (EInt(x));
      |Float(x) :: xs -> parcourt xs (EFloat(x));
  in
  let e = parcourt u V in
  (*On v�rifie le typage*)
  if bon_type e then e
  else failwith"mauvais typage";;



parserparla [Int 32; SUB; Int 5; MUL; LBRACE; Int 61; MOD; Int 7; QUOT; Int 2; RBRACE];;
