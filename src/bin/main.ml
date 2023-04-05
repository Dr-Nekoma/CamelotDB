let () =
  let fc = open_in "examples/insert.cml" in
  let content = try Some(input_line fc) 
                with End_of_file -> None
  in
  match content with
  | None -> print_endline "File not found xD"
  | Some(c) -> match Parse.parse c with
               | None -> print_endline "Could not parse file xD"
               | Some(ast) -> Format.printf "Statement: %a@." pp ast
  print_endline "Hello, World!"

