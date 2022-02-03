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

type variableEnv = lisptype VariableMap.t

(*expect type*)
let wrapperchecktype a b excptn =
  if a == b then a else if b == TypeUndefined then a else excptn a b

let expctype  ?where expect got  = wrapperchecktype  expect got (raise_unexpectedtype  ~where:where)
let doesmatch ?where expect got  = wrapperchecktype expect got (raise_mismatchedtype ~where:where)


module ScopeEnv = struct
    type t = {
    scopetype : lisptype;
    (*Sent downwards and upwards -| ^ inner exp type | V scope type *)
    toinfer : variableEnv;
    (*Sent upwards - ^ variable names are added to the map but then are updated on the way down  *)
    venv : variableEnv;
        (*Sent downwards - V variables are only available in the current scope and inner scopes*)
  }

  let update sc scopetype toinfer venv =
    {
      scopetype = scopetype;
      toinfer = toinfer;
      venv = venv;
    }
  let update_scopetype v sc = 
      update sc v sc.toinfer sc.venv
  let update_toinfer v sc = 
      update sc sc.scopetype v sc.venv
  let update_venv v sc = 
      update sc sc.scopetype sc.toinfer v 
end

let merge_venv a b =
    VariableMap.merge (
    fun k xo yo -> 
        match xo,yo with
        | Some x, Some y -> yo
        | None, yo -> yo
        | xo, None -> xo) a b

let rec wrap se exp: wrapsexp * ScopeEnv.t=
  match exp with
  | Sexp (Symbol a, sexplist) -> funcall se a sexplist 
  | Boolean a -> (BOOLEAN a, ScopeEnv.update_scopetype TypeBoolean se)
  | Symbol a -> (SYMBOL a,   ScopeEnv.update_scopetype TypeSymbol  se)
  | LString a -> (STRING a,  ScopeEnv.update_scopetype TypeString  se)
  | Number a -> (NUMBER a,   ScopeEnv.update_scopetype TypeNumber  se)
  | Unit -> (UNIT, ScopeEnv.update_scopetype TypeUnit se )
  | Sexp (a, b) ->
      (LIST (List.map (fun x -> let (a, _) = (wrap se x) in a) (a::b)), ScopeEnv.update_scopetype TypeList se)

(*
    s - symbol
    l - parameter list
*)
and funcall se s l : wrapsexp * ScopeEnv.t =
  let real_func = Sexp (Symbol s, l) in
  (**check for correct args
    atleast is default to false
   *)
  let cargs ?new_se ?(at_least = false) v =
    let n = List.length l in 
    let se = (match new_se with | Some a -> a | _ -> se) in
    if at_least && n < v then
        (raise_parametermismatchnumber_atleast s v n ~where:(Some(real_func)))
    else if n <> v then 
        (raise_parametermismatchnumber s v n ~where:(Some(real_func)))
    else 
        List.map (wrap se) l
  in

  let variadic scopetype minimum_args=
        let res = cargs ~new_se:(ScopeEnv.update_scopetype scopetype se) ~at_least:true minimum_args in
              List.map (
                  fun (e, se) -> 
                    expctype scopetype (se.scopetype) ~where:(real_func)|> ignore;
                    e
              ) res
        
    in

  let  wrap_and_separate () = 
    let rec wrap_aux res = 
    function
    | hd::tl -> let (e, v) = wrap se hd in 
                let (ares, bres) = res in
                wrap_aux (e::ares, v.scopetype::bres) tl
    | _ -> let (a, b) = res in
           List.rev a, List.rev b
    in wrap_aux ([],[]) l
  in
  match s with
  | "define" ->
      let c = cargs 2 in
      let (vardef_exp, vardef_env), (valres_exp, valres_env) =
        (List.hd c, List.nth c 2)
      in
      let symbol_name = getsymbol_val vardef_exp vardef_env.scopetype in
      let venv = VariableMap.add symbol_name valres_env.scopetype se.venv in
      let se = se |> ScopeEnv.update_venv venv |> ScopeEnv.update_scopetype TypeUnit in
      (DEFINE (vardef_exp, valres_exp), se)

  | "if" ->
      let c = cargs 3 in
      let (cnd_exp, cnd_env), (thn_exp, thn_env), (els_exp, els_env) =
        (List.hd c, List.nth c 1, List.nth c 2)
      in
      expctype ~where:(List.hd l) TypeBoolean cnd_env.scopetype |> ignore;
      let return_type = doesmatch ~where:(List.hd l) thn_env.scopetype els_env.scopetype in


      (IF (cnd_exp, thn_exp, els_exp, return_type), ScopeEnv.update_scopetype return_type se)
  | "lambda" ->
          cargs 2 |> ignore;
          let arg_list = List.hd l |> getvalue_list se |> (List.map (fun x -> 
              let (x_exp, x_env) = wrap se x in 
              getsymbol_val x_exp x_env.scopetype
          )) in
          let se = se |> ScopeEnv.update_toinfer (List.fold_left (fun init a -> VariableMap.add a TypeUndefined init) se.toinfer arg_list)
          in
          let (body_exp, body_env) = wrap se (List.nth l 1) in
          let infered_types = List.map (fun x -> VariableMap.find x body_env.toinfer) arg_list in
            (LAMBDA (body_exp, infered_types, body_env.scopetype), body_env)
  | "map" ->
        let c = cargs 2 in
        let (lambda_exp, lambda_env), (list_exp, list_env) = (List.hd c, List.nth c 2) in
        (match lambda_exp with  
        | LAMBDA(_, arg_list ,_) -> 
                let arg_list_len = List.length arg_list in 
                if arg_list_len <> 1 then 
                    raise_parametermismatchnumber ~where:(Some (List.hd l)) "LAMBDA" 1 arg_list_len
                else 
                    ()
        | _ -> raise_unexpectedtype ~where:(Some (List.hd l)) (TypeLambda([TypeUndefined], TypeUndefined)) lambda_env.scopetype
        );
        (MAP (lambda_exp, list_exp), ScopeEnv.update_scopetype TypeList se)
  | "all" ->
            (ALL(List.map (fun x -> let (exp, _) = wrap se x in exp) l), ScopeEnv.update_scopetype TypeUnit se)
        
  | "quote" -> (
      let (exp, env) = cargs 1 |> List.hd in
      (QUOTE(exp), ScopeEnv.update_scopetype TypeList se)
  )
  | "eval" -> 
          let (exp, env) = cargs 1 |> List.hd in
          (EVAL exp, ScopeEnv.update_scopetype TypeUnit se)
  | "set" ->
      (* TODO: do this but for global functions *)
      let c = cargs 2 in
      let (vardef_exp, vardef_env), (valres_exp, valres_env) =
        (List.hd c, List.nth c 2)
      in
      let symbol_name = getsymbol_val vardef_exp vardef_env.scopetype in
      let venv = VariableMap.add symbol_name valres_env.scopetype se.venv in
      let se = se |> ScopeEnv.update_venv venv |> ScopeEnv.update_scopetype TypeUnit in
      (SET (vardef_exp, valres_exp), se)

  | "load" ->
          let (e, env) = cargs 1 |> List.hd in
          let filename = 
              (match e with
              | STRING a -> a
              | _ -> raise_unexpectedtype ~where:(Some (List.hd l)) TypeString env.scopetype 
              ) in
          (LOAD filename, se)
  (*And they say that Functional programming languages have a nicer syntax*)
  | "*" ->   (MUL (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeNumber se)
  | "+" ->   (SUM (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeNumber se)
  | "-" ->   (SUB (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeNumber se)
  | "/" ->   (DIV (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeNumber se)
  | "%" ->   (MOD (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeNumber se)
  | "max" -> (MAX (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeNumber se)
  | "min" -> (MIN (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeNumber se)
  | "intPart" -> (INTPART ((let (e, env) = cargs 1 |> List.hd in expctype TypeNumber env.scopetype |> ignore; e))
                ,ScopeEnv.update_scopetype TypeNumber se)
  | ">" ->  (GT (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeBoolean se)
  | "<" ->  (LT (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeBoolean se)
  | ">=" -> (GE (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeBoolean se)
  | "<=" -> (LE (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeBoolean se)
  | "=" ->  (EQ (variadic TypeNumber 2), ScopeEnv.update_scopetype TypeBoolean se)
  | "and" ->(AND(variadic TypeBoolean 2), ScopeEnv.update_scopetype TypeBoolean se)
  | "or" -> (OR (variadic TypeBoolean 2), ScopeEnv.update_scopetype TypeBoolean se)
  | "not" ->(NOT(let (e, env) = cargs 1 |> List.hd in expctype TypeBoolean env.scopetype |> ignore; e), ScopeEnv.update_scopetype TypeBoolean se)
  | "begin" -> 
  (
    let rec wrap_begin currentenv currentwrap last_type = 
        function 
            | h::tl -> (
                let (e, v) = wrap currentenv h in
                let new_env = ScopeEnv.update_venv (merge_venv currentenv.venv v.venv ) currentenv in
                wrap_begin new_env (e::currentwrap) v.scopetype tl
            )
            | _ -> (currentwrap, last_type)
    in
    let (res, last_type) = wrap_begin se [] TypeUndefined l in
    (BEGIN(res, last_type), ScopeEnv.update_scopetype last_type se) 
  )

  | "print" -> 
          (
          let arg_amm = List.length l in
          if arg_amm < 1 then 
            raise_parametermismatchnumber_atleast s 1 () 
          else 
            let (exps, types) = wrap_and_separate () in
            (PRINT(exps, types), ScopeEnv.update_scopetype TypeUnit se)
        ) 
  | "inputNumber" ->
          (
          cargs 0  |> ignore;
          (INPUTNUMBER, ScopeEnv.update_scopetype TypeNumber se)
        )
  | "inputString" ->
          (
          cargs 0  |> ignore;
          (INPUTSTRING, ScopeEnv.update_scopetype TypeString se)
        )
  | "toSymbol" -> (TOSYMBOL(cargs 1 |> List.hd |> fst), ScopeEnv.update_scopetype TypeSymbol se)
  | "list" -> (LISTCREATE(List.map (fun x -> wrap se x |> fst) l), ScopeEnv.update_scopetype TypeSymbol se)
  | "car" ->
      let (l, v) = cargs 1 |> List.hd in
      expctype ~where: real_func TypeList v.scopetype |> ignore;
      (CAR(l), ScopeEnv.update_scopetype TypeUndefined se)
  | "cdr" -> 
      let (l, v) = cargs 1 |> List.hd in
      expctype ~where: real_func TypeList v.scopetype |> ignore;
      (CDR(l), ScopeEnv.update_scopetype TypeUndefined se)
  | "cons" ->
        let (a_e, a_v), (l_e, l_v) = (let res = cargs 2 in (List.hd res, List.nth res 1)) in
        expctype ~where: real_func TypeList l_v.scopetype |> ignore;
        (CONS(a_e, l_e), ScopeEnv.update_scopetype TypeList se)

  | "len" -> 
          (let (e ,v) = cargs 1 |> List.hd in
          (LEN(e), ScopeEnv.update_scopetype TypeNumber se)
          )
  | "++" ->
          (
            let (a_e, a_v), (l_e, l_v) = (let res = cargs 2 in (List.hd res, List.nth res 1)) in
            expctype ~where: real_func TypeList a_v.scopetype |> ignore;
            expctype ~where: real_func TypeList l_v.scopetype |> ignore;
            (PLUSPLUS(a_e, l_e), ScopeEnv.update_scopetype TypeList se)
          )
  | "toCharList" -> 
          (
            let (e, v) = cargs 1 |> List.hd in 
            expctype ~where: real_func TypeList v.scopetype |> ignore;
            (TOCHARLIST(e), ScopeEnv.update_scopetype TypeList se)
          )  
  | "toString" -> (
            let (e, v) = cargs 1 |> List.hd in 
            (TOSTRING(e), ScopeEnv.update_scopetype TypeList se)
          )  
  | _ as fnc -> 
        (
            let 

            let infered_types = 
                match VariableMap.find_opt fnc se.toinfer with
                | Some a ->
                | _ -> se.toinfer 

        )
          (*Ver se está na venv, ou se está na infer*)

(**check expected params*)
and listfromsexp = function
  | Sexp (a, b) -> LIST (List.map wrap (a :: b))
  | Unit -> LIST []
  | _ as a -> raise_notalist a

and getsymbol_val sym tp =
  match sym with SYMBOL a -> a | _ -> raise_unexpectedtype TypeSymbol tp

and getvalue_list se =
    function 
    | Sexp(a, b) -> a::b
    | _          -> assert false
    


let prepare program =
  wrap program
    {
      toinfer = Hashtbl.create 10;
      venv = VariableMap.empty;
      penv = ParameterMap.empty;
    }
