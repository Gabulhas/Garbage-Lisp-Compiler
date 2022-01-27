
type program = Program of sexp list

and sexp =
  | Sexp of sexp * sexp list
  | Boolean of bool
  | Symbol of string
  | LString of string
  | LChar of char
  | LList of sexp list
  | Number of number
  | Unit
(*Procedure of procedure*)

and number = Real of float | Integer of int
