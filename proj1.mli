open X86_64
open Ast

val write : string -> program -> unit

val compile : string -> exp -> unit