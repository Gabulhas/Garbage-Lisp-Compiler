open Exceptions
open Preparetyper
open Wrapast
open Format
open Lisptype
open X86_64
open Environments

exception Not_Implemented 

let label_index = ref 0

let new_label_index () =
  label_index := !label_index + 1;
  !label_index

(**
Assuming that all expression return values are stored in RDX
*)

let popn n = addq (imm n) (reg rsp)
let pushn n = subq (imm n) (reg rsp)


let rec binop_fold_generate venv exprs binexp =
    List.fold_left (fun init expr ->
        init ++ 
        pushq (reg rdi) ++
        compile_expr venv expr ++
        popq rsi ++
        binexp

    ) (exprs |> List.hd |> compile_expr venv) (List.tl exprs)

and relational_fold_generate venv exprs jumpexp =
    
      let i = new_label_index () in
      let end_lbl = sprintf "CMP_END_%d" i in
      let false_result = end_lbl ^ "FALSE_RESULT" in
      (List.fold_left (fun init expr ->
          init
          ++ pushq (reg rdi)
          ++ compile_expr venv expr 
          ++ popq rsi
          ++ cmpq (reg rdi) (reg rsi)
          ++ jumpexp false_result
          

      ) (exprs |> List.hd |> compile_expr venv) (List.tl exprs)
      ) 
      ++ movq (imm 1) (reg rdi)
      ++ jmp end_lbl
      ++ label false_result
      ++ movq (imm 0) (reg rdi)
      ++ label end_lbl
      ++ comment ("END-----------------"^ end_lbl)



and print_generate venv exprs types =
    List.fold_left2 ( fun init expr exprtype ->
        init ++
        (match exprtype with
          | TypeBoolean -> raise Not_Implemented
          | TypeSymbol -> raise Not_Implemented
          | TypeString -> raise Not_Implemented
          | TypeNumber -> compile_expr venv expr ++ call "print_int"
          | TypeUnit -> raise Not_Implemented
          | TypeList   -> raise Not_Implemented
          | TypeLambda (_,_)-> raise Not_Implemented
          | TypeUndefined -> raise Not_Implemented
          | TypeFunCall -> raise Not_Implemented
          | ComplexType (_)-> raise Not_Implemented

        )

    ) nop exprs types



and compile_expr (venv:variableEnv) wp =
  (** 
    a + b -> binop a b (exp +)
  *)
  let binop_fold exprs binexp =
      binop_fold_generate venv exprs binexp
  in
  let relational_fold exprs cmpexp =
    relational_fold_generate venv exprs cmpexp
  in

  match wp with

  (*
  (*
    Use the last to generate the index of a variable
   *)
  | DEFINE(vardef_exp, valres_exp) -> 
  | LAMBDA
  | MAP
  | QUOTE
  | EVAL
  | SET
  | LOAD a -> reuse generation_pipeline :)
  *)
  | IF (cond, then_part, else_part, return_type)-> 
      let i = new_label_index () in
      let compiled_then = compile_expr venv then_part in
      let compiled_else = compile_expr venv else_part in
      let if_lbl = sprintf "IF_%d" i in
      let if_end = if_lbl ^ "_END" in
      let if_else = if_lbl ^ "_else" in
      compile_expr venv cond
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ jne if_else 
      ++ compiled_then 
      ++ jmp if_end 
      ++ label if_else
      ++ compiled_else
      ++ label if_end

  (*Binops always have at least 2 elements, so List.tl [] never throws exceptions*)
  | SUM explist -> binop_fold explist (addq (reg rsi) (reg rdi))
  | SUB explist -> binop_fold (List.rev explist) (subq (reg rsi) (reg rdi))
  | MUL explist -> binop_fold explist (imulq (reg rsi) (reg rdi))
  | DIV explist -> binop_fold explist (movq (imm 0) (reg rdx)
                                    ++ movq (reg rsi) (reg rax)
                                    ++ movq (reg rdi) (reg rcx)
                                    ++ idivq (reg rcx)
                                    ++ movq (reg rax) (reg rdi))
  | MOD explist -> binop_fold explist (movq (imm 0) (reg rdx)
                                    ++ movq (reg rsi) (reg rax)
                                    ++ movq (reg rdi) (reg rcx)
                                    ++ idivq (reg rcx)
                                    ++ movq (reg rdx) (reg rdi))

  | GT explist -> relational_fold explist jle
  | LT explist -> relational_fold explist jge
  | GE explist -> relational_fold explist jl
  | LE explist -> relational_fold explist jg
  | EQ explist -> relational_fold explist jne

  | MAX explist ->binop_fold explist (cmpq (reg rsi) (reg rdi) ++ cmovg (reg rsi) rdi)
  | MIN explist ->binop_fold explist (cmpq (reg rsi) (reg rdi) ++ cmovl (reg rsi) rdi)
  | AND explist -> 
      let i = new_label_index () in
      let end_lbl = sprintf "AND_END_%d" i in
      let false_result = end_lbl ^ "FALSE_RESULT" in
      (*
      Lazy AND:
          while the results are equal to 1 we keep going forward (the expression list)
          in case the results are different to 1 (cmpq... jump not equal) we jump to false_result, which
          sets RDI (the return register) to 0 (false)

         if we reach the end of the expression list, the last expression was equal to 1, so RDI has the value 1, so we just
          don't set RDI to 0
      *)
      (List.fold_left (fun init x -> 
          init 
          ++ compile_expr venv x 
          ++ movq (imm 1) (reg rsi)
          ++ cmpq (reg rdi) (reg rsi)
          ++ jne false_result
      ) nop explist)
      ++ je end_lbl ++ label false_result
      ++ movq (imm 0) (reg rdi)
      ++ label end_lbl
  | OR explist ->
      let i = new_label_index () in
      let end_lbl = sprintf "OR_END_%d" i in
      (*
      Lazy AND:
          while no result is equal to 1 we keep going forward (the expression list)
          in case an result is equal to 1 (cmpq... jump equal) we jump to end, which the last expression was equal to 1, so RDI has the value 1, so we just

          if we reach the end of the expression list, and we haven't jumped to the end_lbl, it means that we didn't pass
          any expression which the value was equal to 1, so the expression is false (0) and so we set the rdi (return register)
          to 0
      *)
      (List.fold_left (fun init x -> 
          init 
          ++ compile_expr venv x 
          ++ movq (imm 1) (reg rsi)
          ++ cmpq (reg rdi) (reg rsi)
          ++ je end_lbl
      ) nop explist)
      ++ movq (imm 0) (reg rdi)
      ++ label end_lbl
  | NOT exp ->
      compile_expr venv exp
      ++ movq (imm 1) (reg rsi)
      ++ cmpq (reg rdi) (reg rsi)
      ++ setne (reg dil)
      ++ movzbq (reg dil) rdi
  | ALL (exps, venvs) -> 
          compile_expr venv (BEGIN(exps, TypeUnit, venvs))
          ++ (movq (imm 0) (reg rdi))
  | BEGIN (exps, return_type, venvs)  ->  
          List.fold_left2 (fun init expr this_venv ->
            init ++
            compile_expr this_venv expr
          ) nop exps venvs 
  | PRINT (exps, exps_types)->
    print_generate venv exps exps_types       

  | INPUTNUMBER -> 
           comment "----------INPUTNUMBER----------"
        ++ call "scan_int"
        ++ movq (reg rsi) (reg rdi)
        ++ comment "-------------------------------"
  (*
  | INTPART
  | INPUTSTRING
  | TOSYMBOL
  | LISTCREATE
  | CAR
  | CDR
  | CONS
  | LEN
  | PLUSPLUS
  | TOCHARLIST
  | TOSTRING
  | FUNCTIONCALL
  | LIST
  | SYMBOL
  | STRING
  *)
  | NUMBER a -> 
          (match a with  
          | Real a -> raise Not_Implemented
          | Integer a -> movq (imm a) (reg rdi))
  | BOOLEAN b -> movq (imm (if b then 1 else 0)) (reg rdi)

  (*
  | UNIT
  *)
  | _ -> raise Not_Implemented 

let helper_routines =
  let h_print_int =
    label "print_int"
    ++ pushq (reg rsi)
    ++ call "printInt" 
    ++ popn 8
    ++ ret
  in
  let h_scan_int =
    label "scan_int" 
    ++ pushn 8
    ++ call "inputInt" 
    ++ movq (reg rax) (reg rsi) 
    ++ popn 8
    ++ ret
  in

  nop ++ h_print_int ++ h_scan_int

let helper_data =
    label ".Sprint_int" ++ string "%d\n" ++ label ".SScan_int" ++ string "%d"

let generation_pipeline program =
    let generated_code = compile_expr VariableMap.empty program in
    let p =
        {
            text =
                globl "main" 
        (* exit *)
        ++ helper_routines 
        ++ label "main"
        ++ comment "-------------SETUP-------------"
        ++ movq (reg rsp) (reg rbp)
        ++ movq (imm 0) (reg rax)
        ++ movq (imm 0) (reg rdi)
        ++ comment "-------------------------------"
        ++ generated_code
        ++ comment "-------------EXIT--------------"
        ++ movq (imm 0) (reg rax)
        ++ ret
        ++ comment "-------------------------------";
      data = helper_data;
        }
    in
  p

