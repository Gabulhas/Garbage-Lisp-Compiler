let run_command_get_output command =
  let oc = Unix.open_process_in command in
  let all_input = ref [] in
  try
    while true do
      all_input := input_line oc :: !all_input
    done;
    ""
  with
    End_of_file ->
        close_in oc;
    String.concat "\n" !all_input



let compile_file generated_file_path compiled_binary_path runtime_path = 
    let compile_command = 
        Printf.sprintf "gcc -no-pie -o %s -g %s %s*.s " compiled_binary_path generated_file_path runtime_path in
    Printf.printf "%s\n" compile_command;
    let output = run_command_get_output compile_command in
    if output != "" then
        Printf.printf "GCC Output: '%s' \n" output
