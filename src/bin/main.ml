
open CamelotDB
open Io
open Ast
open Executor

module XML = struct
  let from_string element =
    "<String>" ^ element ^ "</String>"
    |> Ezxmlm.from_string
    |> snd
    |> List.hd
end

let () =
  let fc = open_in "examples/insert.cml" in
  let content = try Some(input_line fc)
                with End_of_file -> None
  in
  match content with
  | None -> print_endline "File not found xD"
  | Some(c) -> match Parse.parse c with
               | None -> print_endline "Could not parse file xD"
               | Some(ast) ->
                  let table = Entity.Table_Info.empty
                              |> Entity.Table_Info.add "Name" ({position = 0; type' = Type.TString {size = 10l}}: Entity.field_metadata)
                              |> Entity.Table_Info.add "Age" ({position = 1; type' = Type.TInteger32}: Entity.field_metadata) in
                  let schema = Schema.Logical_Map.empty
                               |> Schema.Logical_Map.add "Person" (Entity.Table (table, 0l))
                               |> Schema.Logical_Map.add "Abc" (Entity.Table (table, 134l)) in
                  let x = Schema.totally_not_a_list_to_xmlm (Type.primitive_to_xmlm) ([Type.TInteger32]: Type.primitive Schema.totally_not_a_list) in
                  let y = Schema.totally_not_a_tuple_to_xmlm (XML.from_string) (Type.primitive_to_xmlm) (("some_name_here", Type.TInteger32): (String.t,Type.primitive) Schema.totally_not_a_tuple) in

                  let z = Schema.totally_not_a_tuple_of_xmlm_exn () (Type.primitive_of_xmlm) y in
                  let out_c = open_out "/tmp/schema.cms" in
                  (* Schema.serialize schema *)
                  Ezxmlm.to_string [x; y]
                  |> output_string out_c;
                  (* Schema.deserialize "/tmp/schema.cms"; *)
                  (* print_endline (Xmlm.of_string Ast.Type.TInteger32 |> Xmlm.to_string) *)
                  (* execute schema ast |> ignore; *)
                  (* let value = Type.B 10 in *)
                  let value = Type.TInteger32 in
                  let _ =  (Type.primitive_to_xmlm value |> Type.primitive_of_xmlm_exn) in
                  ()

(* let () = *)
(*   let table = Entity.Table_Info.empty |> Entity.Table_Info.add "name" ({position = 0; type' = Type.TString {size = 10}}: Entity.field_metadata) |> Entity.Table_Info.add "age" ({position = 1; type' = Type.TInteger32}: Entity.field_metadata) in *)
(*   let schema = Schema.Logical_Map.empty |> Schema.Logical_Map.add "person" (Entity.Table table) in *)
(*   let row = StringMap.empty |> StringMap.add "name" (Ast.VString "Magueta") |> StringMap.add "age" (Ast.VInteger 24l) in *)
(*   Write.write_row_on_disk schema "person" row; *)

(*
open CamelotDB
open Ast
open Type
open Io
open Io.Write
open Io.Read

let table =
  Entity.Table_Info.empty
  |> Entity.Table_Info.add "name" ({position = 0; type' = Type.TString {size = 10}}: Entity.field_metadata)
  |> Entity.Table_Info.add "age" ({position = 1; type' = Type.TInteger32}: Entity.field_metadata)
  
let nathan_row =
  StringMap.empty
  |> StringMap.add "name" (VString "Nathan")
  |> StringMap.add "age" (VInteger 23l)
  
let schema =
  Schema.Logical_Map.empty
  |> Schema.Logical_Map.add "person" (Entity.Table table)

let serialized_nathan = convert_row schema "person" nathan_row

deserialize schema "person" serialized_nathan

*)
