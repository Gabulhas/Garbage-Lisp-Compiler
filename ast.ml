type number = Real of float | Integer of int

type simpleexp =
  | Sexp of simpleexp * simpleexp list
  | Boolean of bool
  | Symbol of string
  | LString of string
  | Number of number
  | Unit

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
