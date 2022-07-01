open Format
open Printf
open Filename
open Buffer

let program_to_buffer buffer program =
  X86_64.print_program (formatter_of_buffer buffer) program;
  buffer


(*In buffer will contain runtime code*)
let gl_to_code_buffer in_buffer in_channel =
  in_channel 
  |> Lexing.from_channel
  |> Parser.program Lexer.lex
  |> Preparetyper.prepare
  |> Codegeneration.generation_pipeline
  |> program_to_buffer in_buffer
;;

let buffer_to_file buffer filepath =
    let outfile = open_out filepath in
    Printf.fprintf outfile "%s" (Buffer.contents buffer);
    close_out outfile;;

(*let () =
    let result_code = gl_to_code (Buffer.create 16) (open_in Sys.argv.(1)) in
    result_code |> ignore
    *)

let () =
    let input_file = Sys.argv.(1) in
    let machine_code_file = "./out/machinecode/out.s" in
    let output_file_paths = "./out/generated_binaries/a.out" in
    let runtime_path = "./c_runtime_and_stdlib/compiled/" in
    let code_buffer = gl_to_code_buffer (Buffer.create 16) (open_in input_file) in
    buffer_to_file code_buffer machine_code_file;
    Gcccalls.compile_file machine_code_file output_file_paths runtime_path
