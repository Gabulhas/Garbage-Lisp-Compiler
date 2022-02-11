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
