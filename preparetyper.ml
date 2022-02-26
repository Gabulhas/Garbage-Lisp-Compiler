open Ast
open Exceptions
open Environments
open Lisptype
open Wrapast


let rec wrap ?(can_be_symbol=false) se exp: wrapsexp * ScopeEnv.t=
  match exp with
  | Sexp (Symbol a, sexplist) -> funcall se a sexplist 
  | Boolean a -> (BOOLEAN a, ScopeEnv.update_scopetype TypeBoolean se)
  | LString a -> (STRING a,  ScopeEnv.update_scopetype TypeString  se)
  | Number a -> (NUMBER a,   ScopeEnv.update_scopetype TypeNumber  se)
  | Unit -> (UNIT, ScopeEnv.update_scopetype TypeUnit se )
  | Sexp (a, b) ->
    (LIST (List.map (fun x -> let (a, _) = (wrap ~can_be_symbol:true se x) in a) (a::b)), ScopeEnv.update_scopetype TypeList se)
  | Symbol a -> 
    let (result_se, symbol_type) = (
      match VariableMap.find_opt a se.toinfer, VariableMap.find_opt a se.venv with
      | Some ti, None    -> 
        (
          let new_toinfer = VariableMap.update a (function | Some a -> Some se.scopetype | None -> Some se.scopetype ) se.toinfer  in
          (se |> ScopeEnv.update_toinfer new_toinfer,se.scopetype)
        )
      | Some ti, Some df ->  
        (
          let new_toinfer = VariableMap.update a (function | Some a -> Some df | None -> Some df) se.toinfer  in
          (se |> ScopeEnv.update_toinfer new_toinfer, df)
        )
      | None, Some df    -> (se, df)
      | None, None       -> if can_be_symbol then (se, TypeSymbol) else raise_undefined_variable a ~where:exp
    ) in

    (SYMBOL a,   ScopeEnv.update_scopetype symbol_type result_se)

(*
    s - symbol
    l - parameter list
*)
and funcall se s l : wrapsexp * ScopeEnv.t =
  let real_func = Sexp (Symbol s, l) in
  (**check for correct args
     atleast is default to false
  *)
  let cargs ?(can_be_symbol=true) ?new_se ?(at_least = false) v =
    let n = List.length l in 
    let se = (match new_se with | Some a -> a | _ -> se) in
    if at_least && n < v then
      (raise_parametermismatchnumber_atleast s v n ~where:(Some(real_func)))
    else if not at_least && n <> v then 
      (raise_parametermismatchnumber s v n ~where:(Some(real_func)))
    else let (_, result) = List.fold_left_map (fun this_se s -> 
          let (e, v) = wrap this_se s ~can_be_symbol:can_be_symbol in
          let new_se = ScopeEnv.merge_toinfer this_se v in
          (new_se, (e,v))

        ) se l  in
      result
  in

  let variadic scopetype minimum_args=
    let res = cargs ~new_se:(ScopeEnv.update_scopetype scopetype se) ~at_least:true minimum_args in
    let rec wrap_variadic (lc:(wrapsexp * ScopeEnv.t) list) last_se result = 
      match lc with
      | (e, v)::tl -> 
        expctype scopetype (v.scopetype) ~where:(real_func)|> ignore;
        wrap_variadic tl v (e::result)
      | _ -> (List.rev result, last_se)
    in
    let (wrapped, se) = wrap_variadic res se [] in
    (wrapped, se)

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


  let getsymbol_val sym tp =
    match sym with SYMBOL a -> a | _ -> raise_unexpectedtype ~where:(Some(real_func)) TypeSymbol tp 
  in

  match s with
  | "define" ->
    let c = cargs 2 in
    let (vardef_exp, vardef_env), (valres_exp, valres_env) =
      (List.hd c, List.nth c 1)
    in
    let symbol_name = getsymbol_val vardef_exp vardef_env.scopetype in
    let venv = VariableMap.add symbol_name valres_env.scopetype se.venv in
    let se = se |> ScopeEnv.update_venv venv |> ScopeEnv.update_scopetype TypeUnit in
    (DEFINE (vardef_exp, valres_exp), se)

  | "if" ->
    let args_len = List.length l in
    if args_len <> 3 then 
      (raise_parametermismatchnumber s 3 args_len ~where:(Some(real_func)))
    else
    let (cnd_exp, cnd_env) = wrap (ScopeEnv.update_scopetype TypeBoolean se) (List.hd l) in
    let (thn_exp, thn_env) = wrap (ScopeEnv.merge_toinfer se cnd_env)  (List.nth l 1) in
    let (els_exp, els_env) = wrap (ScopeEnv.merge_toinfer se thn_env |> ScopeEnv.update_scopetype thn_env.scopetype)  (List.nth l 2) in

    expctype ~where:(List.hd l) TypeBoolean cnd_env.scopetype |> ignore;
    let return_type = doesmatch ~where:(real_func) thn_env.scopetype els_env.scopetype in


    (IF (cnd_exp, thn_exp, els_exp, return_type), ScopeEnv.merge_toinfer se els_env |> ScopeEnv.update_scopetype return_type)
  | "lambda" ->
    if List.length l <> 2 then 
      raise_parametermismatchnumber s 2 (List.length l) ~where:(Some(real_func))
    else
      let arg_list = List.hd l |> getvalue_list se |> (List.map (fun x -> 
          match x with
          | Symbol a -> a
          | _ -> raise_unexpectedtype TypeSymbol TypeUndefined ~where:(Some(real_func))
        )) in
      let se = se |> ScopeEnv.update_toinfer (List.fold_left (fun init a -> VariableMap.add a TypeUndefined init) se.toinfer arg_list)
      in
      let (body_exp, body_env) = wrap se (List.nth l 1) in
      let infered_types = List.map (fun x -> VariableMap.find x body_env.toinfer) arg_list in
      (LAMBDA (arg_list,body_exp, infered_types, body_env.scopetype), ScopeEnv.update_scopetype (TypeLambda(infered_types, body_env.scopetype)) se)
  | "map" ->
    let c = cargs 2 in
    let (lambda_exp, lambda_env), (list_exp, list_env) = (List.hd c, List.nth c 2) in
    (match lambda_exp with  
     | LAMBDA(_,_, arg_list ,_) -> 
       let arg_list_len = List.length arg_list in 
       if arg_list_len <> 1 then 
         raise_parametermismatchnumber ~where:(Some (List.hd l)) "LAMBDA" 1 arg_list_len
       else 
         ()
     | _ -> raise_unexpectedtype ~where:(Some (List.hd l)) (TypeLambda([TypeUndefined], TypeUndefined)) lambda_env.scopetype
    );
    (MAP (lambda_exp, list_exp), ScopeEnv.update_scopetype TypeList se)
  | "all" ->
    let c = cargs 1 ~at_least:true in
    let exps = List.map (fun (e, v) -> e) c in
    let (_, last_scope) = c |> List.rev |>  List.hd in
    (ALL(exps), ScopeEnv.update_scopetype TypeUnit last_scope)

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
  | "*" ->   let (exps, se) = variadic TypeNumber 2 in (MUL exps, ScopeEnv.update_scopetype TypeNumber se)
  | "+" ->   let (exps, se) = variadic TypeNumber 2 in (SUM exps, ScopeEnv.update_scopetype TypeNumber se)
  | "-" ->   let (exps, se) = variadic TypeNumber 2 in (SUB exps, ScopeEnv.update_scopetype TypeNumber se)
  | "/" ->   let (exps, se) = variadic TypeNumber 2 in (DIV exps, ScopeEnv.update_scopetype TypeNumber se)
  | "%" ->   let (exps, se) = variadic TypeNumber 2 in (MOD exps, ScopeEnv.update_scopetype TypeNumber se)
  | "max" -> let (exps, se) = variadic TypeNumber 2 in (MAX exps, ScopeEnv.update_scopetype TypeNumber se)
  | "min" -> let (exps, se) = variadic TypeNumber 2 in (MIN exps, ScopeEnv.update_scopetype TypeNumber se)
  | "intPart" -> (INTPART ((let (e, env) = cargs 1 |> List.hd in expctype TypeNumber env.scopetype |> ignore; e))
                 ,ScopeEnv.update_scopetype TypeNumber se)
  | ">" ->  let (exps, se) = variadic TypeNumber 2 in (GT exps, ScopeEnv.update_scopetype TypeBoolean se)
  | "<" ->  let (exps, se) = variadic TypeNumber 2 in (LT exps, ScopeEnv.update_scopetype TypeBoolean se)
  | ">=" -> let (exps, se) = variadic TypeNumber 2 in (GE exps, ScopeEnv.update_scopetype TypeBoolean se)
  | "<=" -> let (exps, se) = variadic TypeNumber 2 in (LE exps, ScopeEnv.update_scopetype TypeBoolean se)
  | "=" ->  let (exps, se) = variadic TypeNumber 2 in (EQ exps, ScopeEnv.update_scopetype TypeBoolean se)
  | "and" ->let (exps, se) = variadic TypeBoolean 2 in(AND exps,ScopeEnv.update_scopetype TypeBoolean se)
  | "or" -> let (exps, se) = variadic TypeBoolean 2 in(OR exps, ScopeEnv.update_scopetype TypeBoolean se)
  | "not" ->(NOT(let (e, env) = cargs 1 |> List.hd in expctype TypeBoolean env.scopetype |> ignore; e), ScopeEnv.update_scopetype TypeBoolean se)
  | "begin" -> 
    (
      let rec wrap_begin currentenv currentwrap wrapvenvs last_type = 
        function 
        | h::tl -> (
            let (e, v) = wrap currentenv h in
            let new_env = ScopeEnv.merge_venv (ScopeEnv.merge_toinfer currentenv v) v  in
            wrap_begin new_env (e::currentwrap) (new_env.venv::wrapvenvs) v.scopetype tl)
        | _ -> (currentwrap, last_type, currentenv, wrapvenvs)
      in
      let (res, last_type, last_env, wrapenvs) = wrap_begin se [] [] se.scopetype l in
      (BEGIN(List.rev res, last_type, List.rev wrapenvs), ScopeEnv.merge_toinfer se last_env |> ScopeEnv.update_scopetype last_type ) 
    )

  | "print" -> 
    (
      let arg_amm = List.length l in
      if arg_amm < 1 then 
        raise_parametermismatchnumber_atleast s 0 1 ~where:(Some(real_func))
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
      let func_type_opt = VariableMap.find_opt fnc se.venv in
      if Option.is_some func_type_opt then
        (
          let func_type = Option.get func_type_opt in
          let (arg_types, return_type) = (match func_type with | TypeLambda(a, b) -> (a,b) | _ -> raise_not_a_function ~where:(Some(real_func)) fnc func_type) in
          let (args_len, arg_types_len ) = (List.length l , List.length arg_types) in
          if (args_len != arg_types_len) then 
            raise_parametermismatchnumber fnc arg_types_len args_len ~where:(Some(real_func)) 
            (*TODO: Infer types of arguments*)
          else
            let rec wrap_funcall arg_types args result_exps current_se =
              (
                match arg_types, args with
                | a::tl, a2::tl2 -> 
                  let (res_exp, res_env) = wrap (ScopeEnv.update_scopetype a current_se) a2 in
                  let current_se = ScopeEnv.merge_toinfer current_se res_env in
                  wrap_funcall tl tl2 (res_exp::result_exps) current_se
                | _,_ -> (List.rev result_exps, current_se)
              )
            in
            let (exps, new_env) = wrap_funcall arg_types l [] se in

            (FUNCTIONCALL(fnc, exps), ScopeEnv.update_scopetype return_type new_env)
        )
        (*When the lambda function wasn't defined yet, so, when it's passed as a argument, like

          (define funcA (lambda (my_func) 
                (my_func 4 2 1)
            )
          )
          Here ^, we have to infer the type of my_func's call, this being TypeNumber, TypeNumber, TypeNumber


          TODO: find way to infer return type
        *)
      else
        let (exps, types) = wrap_and_separate () in
        let this_lambda_type = TypeLambda(types, se.scopetype) in
        let some_this_lambda = Some this_lambda_type in
        let to_infer = VariableMap.update fnc (function | Some a -> some_this_lambda | None -> some_this_lambda) se.toinfer in
        let se = ScopeEnv.update_toinfer to_infer se in
        (FUNCTIONCALL(fnc, exps), se)


    )
(*Ver se está na venv, ou se está na infer*)

(**check expected params*)
and getvalue_list se =
  function 
  | Sexp(a, b) -> a::b
  | _          -> assert false

let prepare program =
    let (wrapped, _) = wrap ScopeEnv.empty program 
    in 
    wrapped
