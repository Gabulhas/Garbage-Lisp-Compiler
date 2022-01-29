open Main

let testall () = Sys.readdir "./tests" |> Array.map (fun x -> Printf.printf "\nChecking %s" x; "./tests/" ^ x |> pipeline )
;;
