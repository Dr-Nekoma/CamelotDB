open CamelotDB
open Io.Write
open Ast

(* let () = *)
(*   let fc = open_in "examples/insert.cml" in *)
(*   let content = try Some(input_line fc)  *)
(*                 with End_of_file -> None *)
(*   in *)
(*   match content with *)
(*   | None -> print_endline "File not found xD" *)
(*   | Some(c) -> match Parse.parse c with *)
(*                | None -> print_endline "Could not parse file xD" *)
(*                | Some(ast) -> Format.printf "%a" Ast.pp_statement ast *)

let () =
  let table = Entity.Table_Info.empty |> Entity.Table_Info.add "name" ({position = 0; type' = Type.TString {size = 10}}: Entity.field_metadata) |> Entity.Table_Info.add "age" ({position = 1; type' = Type.TInteger32}: Entity.field_metadata) in
  let schema = Schema.Logical_Map.empty |> Schema.Logical_Map.add "person" (Entity.Table table) in
  let row = StringMap.empty |> StringMap.add "name" (Ast.VString "Magueta") |> StringMap.add "age" (Ast.VInteger 24) in
  write_row_on_disk schema "person" row;
