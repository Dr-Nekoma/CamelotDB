module Write = struct
  open Ast
  open Type
  open Entity

  let convert_table (table: Entity.field_metadata Entity.Table_Info.t) (attribute_name: string) (literal: value) =
    match literal, Entity.Table_Info.find_opt attribute_name table with
    | VString elem, Some {position = _; type' = Type.TString {size}} ->
       let serialized_string = Bytes.of_string elem in
       let serialized_size = Bytes.length serialized_string in
       Bytes.extend serialized_string 0 (size - serialized_size)
    | VInteger elem, Some {position = _; type' = Type.TInteger32} ->
       let serialized_size = Type.to_byte_size Type.TInteger32 in
       let serialized_value = Bytes.create serialized_size in
       Bytes.set_int32_ne serialized_value 0 @@ Int32.of_int(elem);
       serialized_value
    | _ -> failwith "NOT IMPLEMENTED"
  (* let table = Entity.Table_Info.empty |> Entity.Table_Info.add "name" ({position = 0; type' = Type.TString {size = 10}}: Entity.field_metadata) *)
  (* let x = convert_table table "person" (VString "Nathan") *)
  (* let schema () = Schema.Logical_Map.empty |> Schema.Logical_Map.add "person" (Entity.Table table) *)
end

