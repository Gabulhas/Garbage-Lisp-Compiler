
(* The type of tokens. *)

type token = 
  | TRUE
  | RP
  | LSTRING of (string)
  | LP
  | INTVAL of (int)
  | IDE of (string)
  | FLOATVAL of (float)
  | FALSE
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.simpleexp)
