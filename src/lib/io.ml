module Write = struct
  open Ast
  open Type
  open Entity
  open Fsharp

  module StringMap = Map.Make(String)
  type row = Ast.value StringMap.t

  let convert_literal (table: Entity.field_metadata Entity.Table_Info.t) (attribute_name: string) (literal: value) =
    match literal, Entity.Table_Info.find_opt attribute_name table with
    | VString elem, Some {position = pos; type' = Type.TString {size}} ->
       let serialized_string = Bytes.of_string elem in
       let serialized_size = Bytes.length serialized_string in
       let serialized_value = Bytes.extend serialized_string 0 (size - serialized_size) in
       (pos, serialized_value)
    | VInteger elem, Some {position = pos; type' = Type.TInteger32} ->
       print_string "Hello";
       print_newline ();
       let serialized_size = Type.to_byte_size Type.TInteger32 in
       let serialized_value = Bytes.create serialized_size in
       Bytes.set_int32_ne serialized_value 0 @@ Int32.of_int(elem);
       print_bytes serialized_value;
       print_newline ();
       (pos, serialized_value)
    | _ -> failwith "NOT IMPLEMENTED"

  let convert_row (schema: Schema.t) (table_name: string) (row: row) =
    match Schema.Logical_Map.find_opt table_name schema with
    | Some (Table table_info) ->
       StringMap.fold
         (fun key value acc -> convert_literal table_info key value :: acc)
         row
         []
       |> (fun x -> List.iter (fun (_, b) -> print_bytes b) x;x)
       |> List.sort (fun (p1, _) (p2, _) -> compare p1 p2)
       |> List.map snd
       |> Bytes.concat Bytes.empty
    | Some (Procedure) -> failwith "Procedure yet not implemented"
    | None -> Printf.sprintf "Could not find table %s in the schema" table_name |> failwith

  let write_row_on_disk (schema: Schema.t) (table_name: string) (row: row) : unit =
    let serialized_row = convert_row schema table_name row in
    print_bytes serialized_row;
    let out_channel = open_out_bin (Printf.sprintf "/tmp/%s.ndf" table_name) in
    output_bytes out_channel serialized_row;
    close_out out_channel;
end
(*   let table = Entity.Table_Info.empty |> Entity.Table_Info.add "name" ({position = 0; type' = Type.TString {size = 10}}: Entity.field_metadata) *)
(*   let x = convert_literal table "name" (VString "Nathan") *)
(*   let schema () = Schema.Logical_Map.empty |> Schema.Logical_Map.add "person" (Entity.Table table) *)

