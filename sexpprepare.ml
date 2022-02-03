open Utils
open Ast
open List

let hdtl a = hd (tl a)

let rec wrap s =
  match s with
  | Sexp (Symbol a, sexplist) -> funcall a sexplist
  | Boolean a -> BOOLEAN a
  | Symbol a -> SYMBOL a
  | LString a -> STRING a
  | Number a -> NUMBER a
  | Unit -> UNIT
  | Sexp (a, b) -> LIST (List.map wrap (a :: b))

(*
    s - symbol
    l - parameter list
*)
and funcall s l =
  match s with
  | "define" ->
      let c = cep s 2 l in
      DEFINE (hd c, hdtl c)
  | "if" ->
      let c = cep s 3 l in
      IF (hd c, hdtl c, nth c 2)
  | "lambda" ->
      let c = cep s 2 l in
      LAMBDA (hd l |> listfromsexp, hdtl c)
  | "map" ->
      let c = cep s 2 l in
      MAP (hd c, hdtl c)
  | "all" -> ALL (List.map wrap l)
  | "quote" -> QUOTE (cep s 1 l |> hd)
  | "eval" -> EVAL (cep s 1 l |> hd)
  | "set" ->
      let c = cep s 2 l in
      SET (hd c, hdtl c)
  | "load" -> LOAD (cep s 1 l |> hd)
  | "*" -> MUL (cal s 2 l)
  | "+" -> SUM (cal s 2 l)
  | "-" -> SUB (cal s 2 l)
  | "/" -> DIV (cal s 2 l)
  | "%" -> MOD (cal s 2 l)
  | "max" -> MAX (cal s 2 l)
  | "min" -> MIN (cal s 2 l)
  | "intPart" -> INTPART (cep s 1 l |> hd)
  | ">" -> GT (cal s 2 l)
  | "<" -> LT (cal s 2 l)
  | ">=" -> GE (cal s 2 l)
  | "<=" -> LE (cal s 2 l)
  | "=" -> EQ (cal s 2 l)
  | "and" -> AND (cal s 2 l)
  | "or" -> OR (cal s 2 l)
  | "not" -> NOT (cep s 1 l |> hd)
  | "begin" -> BEGIN (cal s 1 l)
  | "print" -> PRINT (map wrap l)
  | "printf" ->
      let c = cal s 1 l in
      PRINTF (hd c, tl c)
  | "printd" -> PRINTD (List.map wrap l)
  | "inputNumber" ->
      cep s 0 l |> ignore;
      INPUTNUMBER
  | "inputString" ->
      cep s 0 l |> ignore;
      INPUTSTRING
  | "readLine" ->
      cep s 0 l |> ignore;
      READLINE
  | "toSymbol" -> TOSYMBOL (cep s 1 l |> hd)
  | "list" -> LISTCREATE (List.map wrap l)
  | "car" -> CAR (cep s 1 l |> hd)
  | "cdr" -> CDR (cep s 1 l |> hd)
  | "cons" ->
      let c = cep s 2 l in
      CONS (hd c, hdtl c)
  | "len" -> LEN (cep s 1 l |> hd)
  | "++" ->
      let c = cep s 2 l in
      PLUSPLUS (hd c, hdtl c)
  | "toCharList" -> TOCHARLIST (cep s 1 l |> hd)
  | "toString" -> TOSTRING (cep s 1 l |> hd)
  | _ as fnc -> FUNCTIONCALL (fnc, map wrap l)

(**check expected params*)
and cep symbol expected params =
  let l = List.length params in
  if l <> expected then raise_parametermismatchnumber symbol expected l params
  else List.map wrap params

(**check at least expected params*)
and cal symbol expected params =
  let l = List.length params in
  if l < expected then
    raise_parametermismatchnumber_atleast symbol expected l params
  else List.map wrap params

and listfromsexp = function
  | Sexp (a, b) -> LIST (List.map wrap (a :: b))
  | Unit -> LIST []
  | _ as a -> raise_notalist a

let prepare program = wrap program
