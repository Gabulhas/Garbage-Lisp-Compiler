open Ast

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
  let rec aux d c =
    match c with
    | Sexp (a, b) ->
        "("
        ^ aux (d + 1) a
        ^ (List.map (fun x -> aux (d + 1) x) b |> String.concat " ")
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

