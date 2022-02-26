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



and compile_expr venv wp =
  (** 
    a + b -> binop a b (exp +)
  *)
  let binop_fold exprs binexp =
      binop_fold_generate venv exprs binexp
  in

  match wp with
  (*
  | DEFINE(vardef_exp, valres_exp) -> 
  | IF
  | LAMBDA
  | MAP
  | ALL
  | QUOTE
  | EVAL
  | SET
  | LOAD
  *)
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
  | GT explist -> binop_fold explist (cmpq (reg rdi) (reg rsi) ++ setg (reg dil) ++ movzbq (reg dil) rdi)
  | LT explist -> binop_fold explist (cmpq (reg rdi) (reg rsi) ++ setl (reg dil) ++ movzbq (reg dil) rdi)
  | GE explist -> binop_fold explist (cmpq (reg rdi) (reg rsi) ++ setge (reg dil) ++ movzbq (reg dil) rdi)
  | LE explist -> binop_fold explist (cmpq (reg rdi) (reg rsi) ++ setle (reg dil) ++ movzbq (reg dil) rdi)
  | EQ explist -> binop_fold explist (cmpq (reg rdi) (reg rsi) ++ sete (reg dil) ++ movzbq (reg dil) rdi)
  | MAX explist ->binop_fold explist (cmpq (reg rsi) (reg rdi) ++ cmovqg (reg rsi) rdi)
  | MIN explist ->binop_fold explist (cmpq (reg rsi) (reg rdi) ++ cmovql (reg rsi) rdi)
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
  | BEGIN (exps, return_type, venvs)  ->  
          List.fold_left2 (fun init expr this_venv ->
            init ++
            compile_expr this_venv expr
          ) nop exps venvs 
  | PRINT (exps, exps_types)->
    print_generate venv exps exps_types       
  (*
  | INTPART
  | INPUTNUMBER
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
  | BOOLEAN
  | SYMBOL
  | STRING
  *)
  | NUMBER a -> 
          (match a with  
          | Real a -> raise (Invalid_argument "Not Implemented yet") 
          | Integer a -> movq (imm a) (reg rdi))
  | BOOLEAN b -> movq (imm (if b then 1 else 0)) (reg rdi)

  (*
  | UNIT
  *)
  | _ -> raise Not_Implemented 

let helper_routines =
  let h_print_int =
    label "print_int"
    ++ movq (reg rdi) (reg rsi)
    ++ movq (ilab ".Sprint_int") (reg rdi)
    ++ movq (imm 0) (reg rax)
    ++ call "printf" ++ ret
  in
  let h_scan_int =
    label "scan_int" ++ pushn 8
    ++ xorl (reg eax) (reg eax)
    ++ movq (ilab ".SScan_int") (reg rdi)
    ++ movq (reg rsp) (reg rsi)
    ++ call "scanf" ++ popq rdi ++ ret
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
        ++ movq (reg rsp) (reg rbp)
        ++ movq (imm 0) (reg rax)
        (* exit *)
        ++ helper_routines 
        ++ label "main"
        ++ generated_code
        ++ ret;
      data = helper_data;
    }
  in
  p

