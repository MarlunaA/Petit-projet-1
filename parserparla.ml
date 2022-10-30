open Lexer

type exp = V 
            | EInt of int | EFloat of float
            |Add of exp * exp |Mul of exp * exp |Sub of exp * exp |Quot of exp * exp |Mod of exp * exp
            |Addf of exp*exp |Mulf of exp*exp |Subf of exp*exp
	      |Cint of exp |Cfloat of exp
	      |Minus of exp |Plus of exp


(*les recherche_pack cherchent dans la suite de la liste l'expression à laquelle va s'appliquer
  respectivement la conversion (int->float ou l'inverse) ou le modulo,
  et renvoie le couple de cet argument avec le reste de la liste*)

let recherche_pack_conv u =
  let rec cherche u compteur v =
    match u with
      |[] -> failwith"mauvais parenthèsage"
      |LBRACE :: xs ->
	if compteur  = 0 then failwith"problème ça ne devrait même pas arriver"
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
      if x <> LBRACE then failwith"une conversion est commencée par une ("
      else cherche xs 1 []

let recherche_pack_mod u =
  let rec cherche u compteur v =
    match u with
      |[] -> failwith"pas bon parenthésage"
      |LBRACE :: xs -> cherche xs (compteur + 1) (LBRACE :: v)
      |RBRACE :: xs ->
	      if (compteur = 1) then (List.rev(RBRACE :: v), xs)
	      else cherche xs (compteur - 1) (RBRACE :: v)
      |x :: xs ->
	cherche xs compteur (x :: v)
  in
  match u with
    |[] -> failwith"mudulo de rien"
    |Int(x) :: xs -> ([Int(x)], xs)
    |LBRACE :: xs -> cherche xs 1 []
    |INT :: xs -> cherche xs 0 [INT]
    |x :: xs ->
      failwith"un modulo est commencé par des parenthèses ou s'applique sur un seul élément entier"

let recherche_pack_unaire u =
  let rec cherche u compteur v =
    match u with
      |[] -> failwith"pas bon parenthésage"
      |LBRACE :: xs ->
	if compteur  = 0 then failwith"problème ça ne devrait même pas arriver"
	else cherche xs (compteur + 1) (LBRACE :: v)
      |RBRACE :: xs ->
	if (compteur = 1) then (List.rev(v), xs)
	else cherche xs (compteur - 1) (RBRACE :: v)
      |x :: xs -> cherche xs compteur (x :: v)
  in
  match u with
    |[] -> failwith"unaire de rien"
    |Int(y) :: xs -> ([Int(y)], xs)
    |Float(y) :: xs -> ([Float(y)], xs)
    |INT :: xs -> cherche xs 1 [INT]
    |FLOAT :: xs -> cherche xs 1 [FLOAT]
    |x :: xs ->
      if x <> LBRACE then failwith"une opération unaire commence par des parenthèses"
      else cherche xs 1 []


(*reparcourt l'arbre final pour vérifier le typage des entiers/flottants*)

let bon_type exp = 
  let rec typ exp =
    match exp with
      |V -> 0
      |Add(e1, e2) ->
	if ((typ e1) <> 0) || ((typ e2) <> 0) then 2
	else 0
      |Addf(e1, e2) ->
	if ((typ e1) <> 1) || ((typ e2) <> 1) then 2
	else 1
      |Mul(e1, e2) ->
	if (typ e1 <> 0) || (typ e2 <> 0) then 2
	else 0
      |Mulf(e1, e2) ->
	if (typ e1 <> 1) || (typ e2 <> 1) then 2
	else 1
      |Sub(e1, e2) ->
	if (typ e1 <> 0) || (typ e2 <> 0) then 2
	else 0
      |Subf(e1, e2) ->
	if (typ e1 <> 1) || (typ e2 <> 1) then 2
	else 1
      |Cfloat(e1) ->
	if (typ e1 <> 0) then 2
	else 1
      |Cint(e1) ->
	if (typ e1 <> 1) then 2
	else 0
      |Quot(e1, e2) ->
	if (typ e1 <> 0) || (typ e2 <> 0) then 2
	else 0
      |Mod(e1, e2) ->
	if (typ e1 <> 0) || (typ e2 <> 0) then 2
	else 0
      |Minus(e1) -> typ e1
      |Plus(e1) -> typ e1
      |EInt(x) -> 0
      |EFloat(x) -> 1
  in
  typ exp
;;


(*fonction principale*)

let parserparla u =
  let rec parcourt u gauche par = (*gauche est l'arbre gauche de la prochaine opération*)
			            (*renvoie un arbre (type exp)*)
    let rec parcourt_droite v compteur vus = (*v est le reste de la liste de lexemes,
					       compteur le compteur de parenthèses,
					       vus la liste des lexemes déjà vus
					       (dont l'AST correspondant deviendra le membre droit de l'arbre dans l'appel qui a appelé parcourt_droite*)
      (*renvoie (expression, liste de lexemes)*)
      match v with
	|[] -> (parcourt (List.rev vus) V par, [])
	|Int(x) :: xs -> parcourt_droite xs compteur (Int(x) :: vus) 
	|Float(x) :: xs -> parcourt_droite xs compteur (Float(x) :: vus) 
	|LBRACE :: xs ->
	  if vus = [] then parcourt_droite xs 1 []
	  else parcourt_droite xs (compteur + 1) (LBRACE :: vus)
	|RBRACE :: xs ->
            if compteur <> 1 then parcourt_droite xs (compteur - 1) (RBRACE :: vus)
            else (parcourt (List.rev vus) V par, xs) 
	|INT :: xs ->
	  parcourt_droite xs compteur (INT :: vus)
	|FLOAT :: xs ->
	  parcourt_droite xs compteur (FLOAT :: vus)
	|MOD :: xs ->
	  parcourt_droite xs compteur (MOD :: vus)
	|PLUS :: xs ->
	  parcourt_droite xs compteur (PLUS :: vus)
	|MINUS :: xs ->
	  parcourt_droite xs compteur (MINUS :: vus)
	|MUL :: xs ->
	  parcourt_droite xs compteur (MUL :: vus)
	|MULF :: xs ->
	  parcourt_droite xs compteur (MULF :: vus)
	|x :: xs ->
	  if compteur = 0 then (parcourt (List.rev vus) V par, v)
	  else parcourt_droite xs compteur (x :: vus)
    in

    (*fonction raccourci utilisée dans parcourt quand on arrive sur une opération*)
    (*On vérifie que la syntaxe est bonne, 
      puis on récupère le sous-arbre droit et le reste de la liste en appelant parcourt_droite,
      et on rappelle parcourt avec le nouvel arbre gauche (g) calculé*)
    let gen xs gauche op =
      if gauche = V then failwith"pas d'argument à gauche"
      else if xs = [] then failwith"pas d'argument à droite"
      else
	let (droit, reste) = parcourt_droite xs 0 [] in (*droit est une exp, 
                                                      reste est xs à laquelle on a tronqué les éléments pris pour faire droit*)
	let g = op (gauche,droit) in (*g est une exp*)
	parcourt reste g par
    in
	  
    match u with
      |[] -> gauche
      |LBRACE :: xs ->
	if xs = [] then failwith"finit pas une parenthèse ouvrante"
	else 
            let (droit, reste) = parcourt_droite u 0 [] in
            parcourt reste droit (par + 1)
      |RBRACE :: xs -> 
            if par <> 1 then failwith "parenthèsage mauvais"
            else gauche;
      |MUL :: xs -> gen xs gauche (function (x, y) -> Mul (x, y))
      |MULF :: xs -> gen xs gauche (function (x, y) -> Mulf (x, y))
      |ADD :: xs -> gen xs gauche (function (x, y) -> Add (x, y))
      |ADDF :: xs -> gen xs gauche (function (x, y) -> Addf (x, y))
      |SUB :: xs -> gen xs gauche (function (x, y) -> Sub (x, y))
      |SUBF :: xs -> gen xs gauche (function (x, y) -> Subf (x, y))
      |QUOT :: xs ->
	let (pack, reste) = recherche_pack_mod xs in
	let g = Quot(gauche, parcourt pack V par) in
	parcourt reste g par
      |MOD :: xs ->
	let (pack, reste) = recherche_pack_mod xs in
	let g = Mod(gauche, parcourt pack V par) in
	parcourt reste g par
      |INT :: xs ->
	let (pack, reste) = recherche_pack_conv xs in
	let g = Cint(parcourt pack V par) in
	parcourt reste g par
      |FLOAT :: xs -> 
	let (pack, reste) = recherche_pack_conv xs in
	let g = Cfloat(parcourt pack V par) in
	parcourt reste g par
      |PLUS :: xs ->
	let (pack, reste) = recherche_pack_unaire xs in
	let g = Plus(parcourt pack V par) in
	parcourt reste g par
      |MINUS :: xs ->
	let (pack, reste) = recherche_pack_unaire xs in
	let g = Minus(parcourt pack V par) in
	parcourt reste g par
      |Int(x) :: xs -> parcourt xs (EInt(x)) par;
      |Float(x) :: xs -> parcourt xs (EFloat(x)) par;
  in
  let e = parcourt u V 0 in
  (*On vérifie le typage*)
  if (bon_type e) < 2 then e
  else failwith"mauvais typage"