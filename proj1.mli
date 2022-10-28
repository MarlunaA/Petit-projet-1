open X86_64
open Parserparla

val code : exp -> (X86_64.data * int) ref -> program 

val write : string -> program -> unit

val final : string -> exp -> unit