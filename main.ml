open Format
open X86_64


let pipeline debug filename =
  open_in filename
  |> Lexing.from_channel
  |> Parser.program Lexer.lex
  |> Preparetyper.prepare
  |> Codegeneration.generation_pipeline
  |> X86_64.print_program
       (if debug then formatter_of_out_channel stdout
       else formatter_of_out_channel (open_out "test.s"))

;;

let () =
    pipeline false Sys.argv.(1)
