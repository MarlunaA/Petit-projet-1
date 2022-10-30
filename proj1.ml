open Ast
open X86_64
open Format


(*Fonctions appelées lors du parcourt de l'arbre*)
let operation op =
  popq rdi ++
  popq rsi ++
  op (reg rdi) (reg rsi) ++
  pushq (reg rsi)

let operation_unaire op =
  popq rdi ++
  op (reg rdi) ++
  pushq (reg rdi)
  
let modulo =
  popq rdi ++
  popq rsi ++
  movq (reg rsi) (reg rax) ++
  cqto ++
  idivq (reg rdi) ++
  pushq (reg rdx)
  
let quotient = 
  popq rdi ++
  popq rsi ++
  movq (reg rsi) (reg rax) ++
  cqto ++
  idivq (reg rdi) ++
  pushq (reg rax)
  
let operationf op =
  movsd (ind rsp) (reg xmm0) ++
  addq (imm 8) (reg rsp) ++
  movsd (ind rsp) (reg xmm1) ++
  addq (imm 8) (reg rsp) ++
  op (reg xmm1) (reg xmm0) ++
  subq (imm 8) (reg rsp) ++
  movsd (reg xmm0) (ind rsp)
  
let moins_unairef =
  movsd (ind rsp) (reg xmm0) ++
  xorpd (reg xmm1) (reg xmm1) ++
  subsd (reg xmm0) (reg xmm1) ++
  movsd (reg xmm1) (ind rsp)

let float_to_int =
  movsd (ind rsp) (reg xmm0) ++
  addq (imm 8) (reg rsp) ++
  cvttsd2si (reg xmm0) (reg rdi) ++
  pushq (reg rdi)

let int_to_float =
  popq rdi ++
  cvtsi2sdq (reg rdi) (reg xmm0) ++
  subq (imm 8) (reg rsp) ++
  movsd (reg xmm0) (ind rsp)

let ajout_float x i =
  (*On l'insère dans la pile*)
  movsd (lab (".F" ^ (string_of_int i))) (reg xmm0) ++
  subq (imm 8) (reg rsp) ++
  movsd (reg xmm0) (ind rsp)


(*Initialise les data en fonction du type de l'expression*)
let deb_data typ =
  if typ = 1 then label "S_float" ++ string "%f\n"
  else label "S_int" ++ string "%d\n"


(*écrit le main du programme assembleur,
   p est la liste des instructions obtenue récursivement en parcourant l'arbre,
   typ est le type du résultat*)
let initialisation p typ =
  if typ = 0 then (*Si le résultat est entier*)
    globl "main" ++
    label "main" ++
    p ++
    popq rdi ++
    call "print_int" ++
    ret ++
    inline "
    print_int:
      movq %rdi, %rsi
      movq $S_int, %rdi
      movq $0, %rax
      call printf
      ret
  "
  else (*Si le résultat est flottant*)
    globl "main" ++
    label "main" ++
    p ++
    movsd (ind rsp) (reg xmm0) ++
    addq (imm 8) (reg rsp) ++
    call "print_float" ++
    ret ++
    inline"
    print_float:
          movq $S_float, %rdi
          movq $1, %rax
          call printf
          ret
          "

  
(*Insère code dans un fichier nommé name*)
let write name code =
  let file = open_out name in
  let fmt = formatter_of_out_channel file in
  print_program fmt code ;
  close_out file


(*Parcourt l'arbre et construit le code final*)

let compile name exp =
  let typ = bon_type exp in
  let data = ref (deb_data (typ), 0) in
  let rec parcourt exp =
    match exp with
      |V -> nop
      |Add(e1, e2) -> parcourt e1 ++ parcourt e2 ++ operation addq
      |Sub(e1, e2) -> parcourt e1 ++ parcourt e2 ++ operation subq
      |Mul(e1, e2) -> parcourt e1 ++ parcourt e2 ++ operation imulq
      |Addf(e1, e2) -> parcourt e1 ++ parcourt e2 ++ operationf addsd
      |Subf(e1, e2) -> parcourt e1 ++ parcourt e2 ++ operationf subsd
      |Mulf(e1, e2) -> parcourt e1 ++ parcourt e2 ++ operationf mulsd
      |Minus(e1) ->
	      if bon_type e1 = 0 then parcourt e1 ++ operation_unaire negq
	      else parcourt e1 ++ moins_unairef
      |Mod(e1, e2) -> parcourt e1 ++ parcourt e2 ++ modulo
      |Quot(e1, e2) -> parcourt e1 ++ parcourt e2 ++ quotient
      |Cint(e1) -> parcourt e1 ++ float_to_int
      |Cfloat(e1) -> parcourt e1 ++ int_to_float
      |EInt(x) -> pushq (imm x)
      |EFloat(x) -> 
        let (dat, i) = !data in
        let new_dat = dat ++ label (".F" ^ (string_of_int i)) ++ inline ("  .double " ^ (string_of_float x) ^ "\n") in
        data := (new_dat, i+1);
        ajout_float x i
  in
  let p = parcourt exp in
  (*Code final*)
  let code =
    {text = 
          initialisation p typ;
    data = 
          fst !data} in
  write name code



