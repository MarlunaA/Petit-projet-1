open Parserparla
open X86_64
open Format

let deb_data typ =
  if typ = 1 then label "S_float" ++ string "%f\n"
  else label "S_int" ++ string "%d\n"

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
  movsd (reg rsp) (reg xmm0) ++
  subq (imm 8) (reg rsp) ++
  movsd (reg rsp) (reg xmm1) ++
  subq (imm 8) (reg rsp) ++
  op (reg xmm1) (reg xmm0) ++
  addq (imm 8) (reg rsp) ++
  movsd (reg xmm0) (reg rsp)
  
let moins_unairef =
  movsd (reg rsp) (reg xmm0) ++
  xorpd (reg xmm1) (reg xmm1) ++
  subsd (reg xmm0) (reg xmm1) ++
  movsd (reg xmm1) (reg rsp)

let float_to_int =
  movsd (reg rsp) (reg xmm0) ++
  subq (imm 8) (reg rsp) ++
  cvttsd2si (reg xmm0) (reg rdi) ++
  pushq (reg rdi)

let int_to_float =
  popq rdi ++
  cvtsi2sdq (reg rdi) (reg xmm0) ++
  addq (imm 8) (reg rsp) ++
  movsd (reg xmm0) (reg rsp)

let ajout_float data x =
  (*On rajoute le flottant dans les data*)
  let (dat, i) = !data in
  let new_dat = dat ++ label (".F" ^ (string_of_int i)) ++ inline ("  .double " ^ (string_of_float x) ^ "\n") in
  data := (new_dat, i+1);
  (*On l'insère dans la pile*)
  movsd (lab (".F" ^ (string_of_int i))) (reg xmm0) ++
  addq (imm 8) (reg rsp) ++
  movsd (reg xmm0) (reg rsp)

let compile exp data =
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
      |Plus(e1) -> parcourt e1
      |Mod(e1, e2) -> parcourt e1 ++ parcourt e2 ++ modulo
      |Quot(e1, e2) -> parcourt e1 ++ parcourt e2 ++ quotient
      |Cint(e1) -> parcourt e1 ++ float_to_int
      |Cfloat(e1) -> parcourt e1 ++ int_to_float
      |EInt(x) -> pushq (imm x)
      |EFloat(x) -> ajout_float data x
        
  in
  if Parserparla.bon_type exp = 0 then (*Si le résultat est entier*)
    parcourt exp ++
    popq rsi ++
    movq (ilab "S_int") (reg rdi) ++
    movq (imm 0) (reg rax) ++
    call "printf"
  else (*Si le résultat est flottant*)
    nop
    (*movsd %rsp, %xmm0*)


let code exp data = 
  {text = globl "main" ++
          label "main" ++
          compile exp data ++
          ret;
  data = 
          fst !data}


let write name code =
  let file = open_out name in
  let fmt = formatter_of_out_channel file in
  print_program fmt code ;
  close_out file

let final name exp =  
  (*couple (data, nombre de flottants vus)*)
  let data = ref (deb_data (bon_type exp), 0) in 
  let code_f = code exp data in
  write name code_f

