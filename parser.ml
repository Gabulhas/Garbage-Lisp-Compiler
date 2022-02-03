
module MenhirBasics = struct
  
  exception Error
  
  let _eRR : exn =
    Error
  
  type token = 
    | TRUE
    | RP
    | LSTRING of (
# 14 "parser.mly"
       (string)
# 16 "parser.ml"
  )
    | LP
    | INTVAL of (
# 12 "parser.mly"
       (int)
# 22 "parser.ml"
  )
    | IDE of (
# 15 "parser.mly"
       (string)
# 27 "parser.ml"
  )
    | FLOATVAL of (
# 13 "parser.mly"
       (float)
# 32 "parser.ml"
  )
    | FALSE
    | EOF
  
end

include MenhirBasics

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState11
  | MenhirState9
  | MenhirState3
  | MenhirState0

# 1 "parser.mly"
   (* HEADER *)

open Ast;;

let program_wrap sexplist = 
    Sexp(
        Symbol "begin",
        sexplist
    )

# 65 "parser.ml"

[@@@ocaml.warning "-4-39"]

let rec _menhir_fail : unit -> 'a =
  fun () ->
    Printf.eprintf "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_sexp_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.simpleexp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_1 : (Ast.simpleexp list)) = _v in
        let _v : (Ast.simpleexp list) = 
# 36 "parser.mly"
                                                        ( _1 )
# 84 "parser.ml"
         in
        _menhir_goto_opt_sexp_list _menhir_env _menhir_stack _menhir_s _v
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_2 : (Ast.simpleexp list)) = _v in
        let (_menhir_stack, _menhir_s, (_1 : (Ast.simpleexp))) = _menhir_stack in
        let _v : (Ast.simpleexp list) = 
# 41 "parser.mly"
                                                        ( _1::_2 )
# 95 "parser.ml"
         in
        _menhir_goto_sexp_list _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_opt_sexp_list : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.simpleexp list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (((_menhir_stack, _menhir_s), _, (_2 : (Ast.simpleexp))), _, (_3 : (Ast.simpleexp list))) = _menhir_stack in
            let _v : (Ast.simpleexp) = 
# 29 "parser.mly"
                                (Sexp(_2, _3))
# 118 "parser.ml"
             in
            _menhir_goto_sexp _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.simpleexp list))) = _menhir_stack in
            let _v : (Ast.simpleexp) = 
# 27 "parser.mly"
                      (program_wrap _1)
# 139 "parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (Ast.simpleexp)) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_goto_sexp : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.simpleexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | FALSE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | FLOATVAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | IDE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | INTVAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | LP ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | LSTRING _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | TRUE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | RP ->
            _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9)
    | MenhirState0 | MenhirState11 | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | FALSE ->
            _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | FLOATVAL _v ->
            _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | IDE _v ->
            _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | INTVAL _v ->
            _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | LP ->
            _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | LSTRING _v ->
            _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
        | TRUE ->
            _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11
        | EOF | RP ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (_1 : (Ast.simpleexp))) = _menhir_stack in
            let _v : (Ast.simpleexp list) = 
# 40 "parser.mly"
                                                        ( [_1] )
# 208 "parser.ml"
             in
            _menhir_goto_sexp_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_atom : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.simpleexp) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (Ast.simpleexp)) = _v in
    let _v : (Ast.simpleexp) = 
# 31 "parser.mly"
                                (_1)
# 220 "parser.ml"
     in
    _menhir_goto_sexp _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState9 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.simpleexp list) = 
# 35 "parser.mly"
                                                        ( [] )
# 248 "parser.ml"
     in
    _menhir_goto_opt_sexp_list _menhir_env _menhir_stack _menhir_s _v

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpleexp) = 
# 50 "parser.mly"
                (Boolean true)
# 259 "parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 14 "parser.mly"
       (string)
# 266 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 14 "parser.mly"
       (string)
# 274 "parser.ml"
    )) = _v in
    let _v : (Ast.simpleexp) = 
# 48 "parser.mly"
                (LString(_1))
# 279 "parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | FLOATVAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | IDE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | INTVAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | LSTRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | RP ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_s = MenhirState3 in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _v : (Ast.simpleexp) = 
# 30 "parser.mly"
             (Unit)
# 310 "parser.ml"
         in
        _menhir_goto_sexp _menhir_env _menhir_stack _menhir_s _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 12 "parser.mly"
       (int)
# 323 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 12 "parser.mly"
       (int)
# 331 "parser.ml"
    )) = _v in
    let _v : (Ast.simpleexp) = 
# 46 "parser.mly"
                (Number (Integer (_1)))
# 336 "parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 15 "parser.mly"
       (string)
# 343 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 15 "parser.mly"
       (string)
# 351 "parser.ml"
    )) = _v in
    let _v : (Ast.simpleexp) = 
# 49 "parser.mly"
                (Symbol(_1))
# 356 "parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 13 "parser.mly"
       (float)
# 363 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (_1 : (
# 13 "parser.mly"
       (float)
# 371 "parser.ml"
    )) = _v in
    let _v : (Ast.simpleexp) = 
# 47 "parser.mly"
                (Number (Real (_1)))
# 376 "parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _v : (Ast.simpleexp) = 
# 51 "parser.mly"
                (Boolean false)
# 387 "parser.ml"
     in
    _menhir_goto_atom _menhir_env _menhir_stack _menhir_s _v

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and program : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.simpleexp) =
  fun lexer lexbuf ->
    let _menhir_env = {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = Obj.magic ();
      _menhir_error = false;
    } in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FLOATVAL _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | IDE _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INTVAL _v ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LP ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LSTRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | EOF ->
        _menhir_reduce7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)
