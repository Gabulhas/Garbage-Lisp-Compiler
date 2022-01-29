%{ (* HEADER *)

open Ast;;

let program_wrap sexplist = 
    Sexp(
        Symbol "begin",
        sexplist
    )
%}

%token <int>        INTVAL
%token <float>      FLOATVAL
%token <string>     LSTRING
%token <char>       LCHAR
%token <string>     IDE
%token              TRUE FALSE

%token              EOF
%token              LP RP


%start program
%type <Ast.sexp> program
%%

program:
    opt_sexp_list EOF {program_wrap $1}
sexp:
     | LP sexp opt_sexp_list RP {Sexp($2, $3)}
     | LP RP {Unit}
     | atom                     {$1}
     ;

opt_sexp_list:
    |                                                   { [] }
    | sexp_list                                         { $1 }
    ;

sexp_list: 
    | sexp                                              { [$1] }
    | sexp  sexp_list                                   { $1::$2 }
    ;


atom:
    | INTVAL    {Number (Integer ($1))}
    | FLOATVAL  {Number (Real ($1))}
    | LSTRING   {LString($1)}
    | IDE       {Symbol($1)}
    | TRUE      {Boolean true}
    | FALSE     {Boolean false}
