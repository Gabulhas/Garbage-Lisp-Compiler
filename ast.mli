type number = Real of float | Integer of int

type sexp =
  | Sexp of sexp * sexp list
  | Boolean of bool
  | Symbol of string
  | LString of string
  | Number of number
  | Unit

type lisptypes =
  | TypeBoolean
  | TypeSymbol
  | TypeString
  | TypeNumber
  | TypeUnit
  | TypeList
   (**Args types, return type *)
  | TypeLambda of lisptypes list * lisptypes
  | TypeUndefined 
  | ComplexType of lisptypes list

type wrapsexp =
  (**"define" *)
  | DEFINE of wrapsexp * wrapsexp
  (* "if" *)
  | IF of wrapsexp * wrapsexp * wrapsexp
  (* "lambda" *)
  | LAMBDA of wrapsexp * wrapsexp
  (* "map" *)
  | MAP of wrapsexp * wrapsexp
  (* "all" *)
  | ALL of wrapsexp list
  (* "quote" *)
  | QUOTE of wrapsexp
  (* "eval" *)
  | EVAL of wrapsexp
  (* "set!" *)
  | SET of wrapsexp * wrapsexp
  (* "load" *)
  | LOAD of wrapsexp
  (* "*" *)
  | MUL of wrapsexp list
  (* "+" *)
  | SUM of wrapsexp list
  (* "-" *)
  | SUB of wrapsexp list
  (* "/" *)
  | DIV of wrapsexp list
  (* "%" *)
  | MOD of wrapsexp list
  (* "max" *)
  | MAX of wrapsexp list
  (* "min" *)
  | MIN of wrapsexp list
  (* "intPart" *)
  | INTPART of wrapsexp
  (* ">" *)
  | GT of wrapsexp list
  (* "<" *)
  | LT of wrapsexp list
  (* ">=" *)
  | GE of wrapsexp list
  (* "<=" *)
  | LE of wrapsexp list
  (* "=" *)
  | EQ of wrapsexp list
  (* "and" *)
  | AND of wrapsexp list
  (* "or" *)
  | OR of wrapsexp list
  (* "not" *)
  | NOT of wrapsexp
  (* "begin" *)
  | BEGIN of wrapsexp list
  (* "print" *)
  | PRINT of wrapsexp list
  (* "printf" *)
  | PRINTF of wrapsexp * wrapsexp list
  (* "printd" *)
  | PRINTD of wrapsexp list
  (* "inputNumber" *)
  | INPUTNUMBER
  (* "inputString" *)
  | INPUTSTRING
  (* "readLine" *)
  | READLINE
  (* "toSymbol" *)
  | TOSYMBOL of wrapsexp
  (* "list" *)
  | LISTCREATE of wrapsexp list
  (* "car" *)
  | CAR of wrapsexp
  (* "cdr" *)
  | CDR of wrapsexp
  (* "cons" *)
  | CONS of wrapsexp * wrapsexp
  (* "len" *)
  | LEN of wrapsexp
  (* "++" *)
  | PLUSPLUS of wrapsexp * wrapsexp
  (* "empty?" *)
  | ISEMPTY of wrapsexp
  (* "list?" *)
  | ISLIST of wrapsexp
  (* "procedure?" *)
  | ISPROCEDURE of wrapsexp
  (* "symbol?" *)
  | ISSYMBOL of wrapsexp
  (* "bool?" *)
  | ISBOOL of wrapsexp
  (* "number?" *)
  | ISNUMBER of wrapsexp
  (* "string?" *)
  | ISSTRING of wrapsexp
  (* "equals?" *)
  | ISEQUALS of wrapsexp
  (* "type?" *)
  | ISTYPE of wrapsexp
  (* "toCharList" *)
  | TOCHARLIST of wrapsexp
  (* "toString" *)
  | TOSTRING of wrapsexp
  (* SYMBOL ...    *)
  | FUNCTIONCALL of string * wrapsexp list
  (*Example (list (4 5 6)), where (4 5 6) is an exp that doesn't start with symbol*)
  | LIST of wrapsexp list
  | BOOLEAN of bool
  | SYMBOL of string
  | STRING of string
  | NUMBER of number
  | UNIT
