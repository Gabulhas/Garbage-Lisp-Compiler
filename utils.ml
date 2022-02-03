open Ast

exception ParameterMismatchNumber of string
exception NotAList of string
exception UnexpectedType of string
exception MismatchedType of string

let rec type_to_string = function
  | TypeBoolean -> "Boolean"
  | TypeSymbol -> "Symbol"
  | TypeString -> "String"
  | TypeNumber -> "Number"
  | TypeUnit -> "Unit"
  | TypeList -> "List"
  | TypeFunCall -> "Funcall"
  | TypeLambda (argtypes, returntype) ->
      let types =
        List.map type_to_string (argtypes @ [ returntype ])
        |> String.concat " -> "
      in
      Printf.sprintf "Lambda: %s" types
  | TypeUndefined -> "Undefined"
  | ComplexType a ->
      "[" ^ (List.map type_to_string a |> String.concat ", ") ^ "]"

let sexp_to_string s =
  let rec depth_tabber depth =
    if depth > 0 then "   " ^ depth_tabber (depth - 1) else ""
  in
  let rec aux d c =
    depth_tabber d
    ^
    match c with
    | Sexp (a, b) ->
        "("
        ^ aux (d + 1) a
        ^ (List.map (fun x -> " " ^ aux (d + 1) x) b |> String.concat "")
        ^ ")"
    | Boolean a -> if a then "#t" else "#f"
    | Symbol a -> a
    | LString a -> Printf.sprintf "\"%s\"" a
    | Number a -> (
        match a with
        | Real a -> string_of_float a
        | Integer a -> string_of_int a)
    | Unit -> "()"
  in

  aux 0 s

let here_text = function Some a -> "Here: " ^ sexp_to_string a | None -> ""

(* TODO: merge raise_parametermismatchnumber and raise_parametermismatchnumber_atleast


*)

let raise_parametermismatchnumber symbol expected got ?(where = None) =
  raise
    (ParameterMismatchNumber
       (Printf.sprintf
          "Function \"%s\" expected %d arguments, but got %d instead. %s" symbol
          expected got (here_text where)))

let raise_parametermismatchnumber_atleast symbol expected got ?(where = None) =
  raise
    (ParameterMismatchNumber
       (Printf.sprintf
          "Function \"%s\" expected at least %d arguments, but got %d instead. \
           %s"
          symbol expected got (here_text where)))

let raise_notalist l =
  raise
    (NotAList (Printf.sprintf "Expression %s is not a list." (sexp_to_string l)))

let raise_unexpectedtype expected got ?(where = None) =
  raise
    (UnexpectedType
       (Printf.sprintf "Expected %s but got %s. %s" (type_to_string expected)
          (type_to_string got) (here_text where)))

let raise_mismatchedtype a b ?(where = None) =
  raise
    (UnexpectedType
       (Printf.sprintf "Type Mismatch: %s =/= %s. %s" (type_to_string a)
          (type_to_string b) (here_text where)))
