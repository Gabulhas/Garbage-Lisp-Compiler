(**
Checks types
Infers types
*)
open Ast
open Utils

(**Maps Variable Name to Type*)
module VariableMap = Map.Make (struct
  type t = string
  let compare = compare
end)

(**Maps Funcion Parameters to Types for each parameter*)
module ParameterMap = Map.Make (struct
  type t = string
  let compare = compare
end)


let check program = ""

let cmp_tp a b =
  let rec aux x y =
    match (x, y) with
    | ComplexType c, ComplexType d -> List.equal aux c d
    | ComplexType c, _ -> false
    | _, ComplexType d -> false
    | _, _ -> x = y
  in
  aux a b 

(*expect type*)
let wrapperchecktype a b excptn =
    if cmp_tp a b then a
    else
        if b = TypeUndefined then
            a
        else
            excptn a b |> raise
let expctype a b =
    wrapperchecktype a b raise_unexpectedtype

let doesmatch a b =
    wrapperchecktype a b raise_unexpectedtype


(*Type*VM*PM*)
(** Accepts sexpression, variable map, parameter map.*)
let rec check_exp sexp vm pm=

    (** Util function to avoid repetition*)
    let rec chk a = check_exp a vm pm 
    in
    match sexp with
  | DEFINE(a, b) -> 
    let symbol = getsymbol chk a in
    let (vm, pm) = (
        match b with
        | LAMBDA(a,b) -> (vm, 
            let typelist = List.map (fun x -> let (t,_,_) = chk x in t) (getlistcontent chk b)
            (*isto está errado porque vai devolver tipos symbol, e nao os tipos realmente usados na funcao *)
            in ParameterMap.add symbol typelist pm
        )
        | _ -> (let (t, _, _) = chk a in VariableMap.add symbol t vm, pm)
    )
    in
        
    (TypeUnit, vm, pm);

  
    | IF(cnd, thn, els) -> 
            expctype TypeBoolean (gettypeonly chk cnd) |> ignore;
            let thn_t = (gettypeonly chk thn) in
            let els_t = (gettypeonly chk els) in
            doesmatch thn_t els_t |> ignore;
            (thn_t, vm, pm)

  

  (*
    *)

  
  | MAP(fnc, lst) -> 
        (*TODO*)
        (match gettypeonly chk fnc with 
            | TypeLambda(argtypes, _) when List.length argtypes = 1->  ()
            | _ as a-> raise_unexpectedtype (TypeLambda([TypeUndefined], TypeUndefined)) a

        );
        expctype TypeList (gettypeonly chk lst) |> ignore;
        (TypeList, vm, pm)

  (*TODO this might be wrong*)
  | QUOTE q -> 
          (TypeList , vm, pm)
  

  | EVAL q -> 
          (gettypeonly chk q, vm, pm)
  

  (*
  (*TODO same as Define but for global vars*)
  | SET of wrapsexp * wrapsexp
  *)
  | LOAD l ->
          expctype TypeString (gettypeonly chk l) |> ignore;
          (TypeUnit, vm, pm)


  (*TODO fix code duplication*)
  | MUL lst
  | SUM lst 
  | SUB lst
  | DIV lst
  | MOD lst
  | MAX lst
  | MIN lst -> List.map (fun x -> expctype TypeNumber (gettypeonly chk x)) lst |> ignore; 
               (TypeNumber, vm, pm)
  
  | INTPART a -> expctype TypeNumber (gettypeonly chk a) |> ignore;
                (TypeNumber, vm, pm)
  
  | GT  lst
  | LT  lst
  | GE  lst
  | LE  lst
  | EQ  lst -> List.map (fun x -> expctype TypeNumber (gettypeonly chk x)) lst |> ignore; 
               (TypeBoolean, vm, pm)  

  | AND lst
  | OR  lst -> List.map (fun x -> expctype TypeBoolean (gettypeonly chk x)) lst |> ignore; 
               (TypeBoolean, vm, pm)  


  | NOT n -> expctype TypeBoolean (gettypeonly chk n) |> ignore;
             (TypeBoolean, vm, pm)

  | ALL lst
  | BEGIN lst ->
    let mergeaux k c l =
        match c, l with
        | Some x, Some y -> Some y
        | None, l -> l
        | c, None -> c
    in
    (*
    clst -> current list of exps (for recursion)
    cltype -> current last type
    cvm -> current variable map
    cpm -> current parameter map
     *)
    let rec chkall clst cltype cvm cpm =
        match clst with
        | c :: tl -> let (ltype, tvm, tpm) = check_exp c cvm cpm in 
                     let cvm = VariableMap.merge mergeaux cvm tvm in
                     let cpm = ParameterMap.merge mergeaux cpm tpm in
                     chkall tl ltype cvm cpm 
                     
        | _ -> (cltype, cvm, cpm)

    in
    let (lasttype, vm, pm) = chkall lst TypeUnit vm pm in
  
      let result = (
      match sexp with
      | ALL a -> (TypeUnit, vm, pm)
      | BEGIN a -> (lasttype, vm, pm)
      | _ -> assert false
      )
  in result 
  
  | PRINT lst -> 
          List.map chk lst |> ignore;
          (TypeUnit, vm, pm)
  
  | PRINTF(fmt, rst) -> 
          expctype TypeString (gettypeonly chk fmt) |> ignore;
          List.map chk rst |> ignore;
          (TypeUnit, vm, pm)
  
  | PRINTD lst ->
          List.map chk lst |> ignore;
          (TypeUnit, vm, pm)
  
  | INPUTNUMBER -> 
          (TypeNumber, vm, pm)
  
  | INPUTSTRING 
  | READLINE ->
          (TypeString, vm, pm)
  
  
  | TOSYMBOL a ->
          expctype TypeString (gettypeonly chk a) |> ignore;
          (TypeSymbol, vm, pm)
  
  | LISTCREATE lst ->   
          List.map chk lst |> ignore;
          (TypeList, vm, pm)
  
  | CAR l ->
          expctype TypeList (gettypeonly chk l) |> ignore;
          (TypeUndefined, vm, pm)
          
  | CDR l -> 
      expctype TypeList (gettypeonly chk l) |> ignore;
      (TypeList, vm, pm)
  
  | CONS (f,l) -> 
          chk f |> ignore;
          chk l |> ignore;
          (TypeList, vm, pm)
  
  | LEN a -> 
      expctype TypeList (gettypeonly chk a) |> ignore;
      (TypeNumber, vm, pm)
  
  | PLUSPLUS (f,l) -> 
          chk f |> ignore;
          chk l |> ignore;
          (TypeList, vm, pm)
  

  (*TODO: Might be removed, since its a type lisp*)
  | ISEMPTY e
  | ISLIST e
  | ISPROCEDURE e
  | ISSYMBOL e
  | ISBOOL e
  | ISNUMBER e
  | ISSTRING e
  | ISEQUALS e
  | ISTYPE e ->
          chk e |> ignore;
          (TypeBoolean, vm, pm)

  | TOCHARLIST s -> 
      expctype TypeString (gettypeonly chk s) |> ignore;
      (TypeList, vm, pm)
  | TOSTRING e ->
      chk e |> ignore;
      (TypeString, vm, pm)


  | LIST lst -> 
          List.map chk lst |> ignore;
          (TypeList, vm, pm)

  | BOOLEAN _ -> (TypeBoolean, vm, pm)

  | STRING _ -> (TypeString, vm, pm)
  | NUMBER _ -> (TypeNumber, vm, pm)
  | UNIT ->  (TypeUnit, vm, pm)
  (*
  | LAMBDA(args,body) -> 
    (*TODO type inference*)
    getlistcontent 
  | FUNCTIONCALL (name, args) -> 
  | SYMBOL a -> (*Check if symbol is in variable env*)
  *)
  | _ -> assert false;
and gettypeonly chkfun a =
    let (tp, _, _ ) = chkfun a in
    tp
and getsymbol chkfun a =
    match a with
    | SYMBOL a -> a
    | _ -> raise_unexpectedtype TypeSymbol (gettypeonly chkfun a) 

and getlambdaparams chkfun a =
    match a with
    | LAMBDA(a,b) -> a 
    | _ -> assert false

and getlambdabody chkfun a =
    match a with
    | LAMBDA(a,b) -> b
    | _ -> assert false

and getlistcontent chkfun a =
    match a with
    | LIST a -> a
    | _ -> raise_unexpectedtype TypeList (gettypeonly chkfun a)
