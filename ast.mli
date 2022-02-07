type number = Real of float | Integer of int

type simpleexp =
  | Sexp of simpleexp * simpleexp list
  | Boolean of bool
  | Symbol of string
  | LString of string
  | Number of number
  | Unit

type lisptype =
  | TypeBoolean
  | TypeSymbol
  | TypeString
  | TypeNumber
  | TypeUnit
  | TypeList  (**Args types, return type *)
  | TypeLambda of lisptype list * lisptype
  | TypeUndefined
  | TypeFunCall
  | ComplexType of lisptype list

type wrapsexp =
  | DEFINE of wrapsexp * wrapsexp
  (*If in Lisp (or at least Garbage Lisp) acts as a function*)
  | IF of wrapsexp * wrapsexp * wrapsexp * lisptype
  (*Parameters Type and Return Type*)
  | LAMBDA of string list * wrapsexp * lisptype list * lisptype
  | MAP of wrapsexp * wrapsexp
  | ALL of wrapsexp list
  | QUOTE of wrapsexp
  | EVAL of wrapsexp
  | SET of wrapsexp * wrapsexp
  | LOAD of string
  | MUL of wrapsexp list
  | SUM of wrapsexp list
  | SUB of wrapsexp list
  | DIV of wrapsexp list
  | MOD of wrapsexp list
  | MAX of wrapsexp list
  | MIN of wrapsexp list
  | INTPART of wrapsexp
  | GT of wrapsexp list
  | LT of wrapsexp list
  | GE of wrapsexp list
  | LE of wrapsexp list
  | EQ of wrapsexp list
  | AND of wrapsexp list
  | OR of wrapsexp list
  | NOT of wrapsexp
  (*Return Type*)
  | BEGIN of wrapsexp list * lisptype
  | PRINT of wrapsexp list * lisptype list
  | INPUTNUMBER
  | INPUTSTRING
  | TOSYMBOL of wrapsexp
  | LISTCREATE of wrapsexp list
  | CAR of wrapsexp
  | CDR of wrapsexp
  | CONS of wrapsexp * wrapsexp
  | LEN of wrapsexp
  | PLUSPLUS of wrapsexp * wrapsexp
  | TOCHARLIST of wrapsexp
  | TOSTRING of wrapsexp
  | FUNCTIONCALL of string * wrapsexp list
  | LIST of wrapsexp list
  | BOOLEAN of bool
  | SYMBOL of string
  | STRING of string
  | NUMBER of number
  | UNIT
