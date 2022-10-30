type exp = V 
          | EInt of int | EFloat of float
          |Add of exp * exp |Mul of exp * exp |Sub of exp * exp |Quot of exp * exp |Mod of exp * exp
          |Addf of exp*exp |Mulf of exp*exp |Subf of exp*exp
	        |Cint of exp |Cfloat of exp
	        |Minus of exp

(*0 -> int
   1 -> float
   _ -> erreur*)
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
      |EInt(x) -> 0
      |EFloat(x) -> 1
  in
  typ exp