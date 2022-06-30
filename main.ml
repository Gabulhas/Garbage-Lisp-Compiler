open Format
open X86_64
open Printf
open Sys
open Filename

let pipeline filepath =
  let filename = basename filepath in
  let result_generated = sprintf "./out/machinecode/%s.s" filename in
  let result_gcompiled = sprintf "./out/generated_binaries/%s.out" filename in
  open_in filepath
  |> Lexing.from_channel
  |> Parser.program Lexer.lex
  |> Preparetyper.prepare
  |> Codegeneration.generation_pipeline
  |> X86_64.print_program
       (formatter_of_out_channel (open_out result_generated));

  let compile_command = 
      sprintf "gcc -no-pie -g %s -o %s" result_generated result_gcompiled in

  printf "Compile command: %s\n" compile_command;

  
  Sys.command compile_command

;;

let () =
    pipeline Sys.argv.(1) 
    |> exit
