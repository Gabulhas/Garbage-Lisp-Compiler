{ (* HEADER *)

open Parser;;

exception UnknownChar;;

let remove_quotes my_str =
    String.sub my_str 1 ((String.length my_str) -2)

let print_lex lx =
    print_string lx;;

}

let digit = ['0' - '9']
let intval = '-'? digit+
let floatval = digit+ ['.'] digit+
let letter = [ 'a'-'z' 'A' - 'Z']
let specialchar  = ('+' |'-' |'/'|'%'|'<'|">="|"<="|'='|'?')+
let ident = (letter | specialchar) ( letter | digit | '_' | '-')*
let character = (digit | letter)
let string = '"' [^'"']* '"'


let space = [' ' '\t']
let comment = "#" [^'\n']*


(* RULES *)

rule lex =
    parse intval        { INTVAL  (int_of_string   (Lexing.lexeme lexbuf))  }
      | floatval        { FLOATVAL(float_of_string (Lexing.lexeme lexbuf))    }
      | string          { LSTRING (remove_quotes (Lexing.lexeme lexbuf))      }
      | character       { LCHAR ( String.get (Lexing.lexeme lexbuf) 0)      }
      | ident           { IDE (Lexing.lexeme lexbuf)}
      | "#t"            {TRUE}
      | "#f"            {FALSE}
      | "("             { LP }
      | ")"             { RP }

      | [' ' '\t' '\n'] { lex lexbuf }
      | eof             { EOF }

      | _               {print_lex (Lexing.lexeme lexbuf); EOF}

