
let pipeline filename =
  open_in filename
  |> Lexing.from_channel
  |> Parser.program Lexer.lex
  |> Preparetyper.prepare
;;
