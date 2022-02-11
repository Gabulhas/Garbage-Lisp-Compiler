open Lisptype
open Ast

exception ParameterMismatchNumber of string
exception NotAList of string
exception UnexpectedType of string
exception MismatchedType of string
exception MismatchedArgumentTypes of string
exception NotAFunction of string
exception UndefinedVariable of string
exception InferError of string

let here_text = function Some a -> "Here: " ^ sexp_to_string a | None -> ""

(* TODO: merge raise_parametermismatchnumber and raise_parametermismatchnumber_atleast*)
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

let raise_not_a_function symb current_type ?(where = None) =
  raise
    (UnexpectedType
       (Printf.sprintf "%s is not a Lambda/Function, it's Type %s. %s" symb
          (type_to_string current_type)
          (here_text where)))

let raise_mismatched_argument_types symb real_args wrong_args ?(where = None) =
  let real_args_str = ComplexType real_args |> type_to_string in
  let wrong_args_str = ComplexType wrong_args |> type_to_string in
  raise
    (MismatchedArgumentTypes
       (Printf.sprintf "Mismatched argument types when calling %s: %s =/=%s. %s"
          symb real_args_str wrong_args_str (here_text where)))

let raise_undefined_variable symb ?where =
  raise
    (MismatchedArgumentTypes
       (Printf.sprintf "Undefined variable %s: %s" symb (here_text where)))

let raise_infer_error a b ?where =
  raise
    (InferError
       (Printf.sprintf "Unable to infer different types: %s =/= %s. %s"
          (type_to_string a) (type_to_string b) (here_text where)))



(*expect type*)
let wrapperchecktype a b excptn =
  if a = b then a else if b == TypeUndefined then a else excptn a b

let expctype  ?where expect got  = wrapperchecktype  expect got (raise_unexpectedtype  ~where:where)
let doesmatch ?where expect got  = wrapperchecktype expect got (raise_mismatchedtype ~where:where)


