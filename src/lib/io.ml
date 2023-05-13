module StringMap = Map.Make(String)

module Write = struct
  open Ast
  open Type
  open Entity

  type row = Ast.value StringMap.t

  let convert_literal (table: Entity.field_metadata Entity.Table_Info.t) (attribute_name: string) (literal: value) =
    match literal, Entity.Table_Info.find_opt attribute_name table with
    | VString elem, Some {position = pos; type' = Type.TString {size}} ->
       let serialized_string = Bytes.of_string elem in
       let serialized_size = Bytes.length serialized_string in
       let serialized_value = Bytes.extend serialized_string 0 ((size |> Int32.to_int) - serialized_size) in
       (pos, serialized_value)
    | VInteger elem, Some {position = pos; type' = Type.TInteger32} ->
       print_string "Hello";
       print_newline ();
       let serialized_size = Type.to_byte_size Type.TInteger32 in
       let serialized_value = Bytes.create (Int32.to_int serialized_size) in
       Bytes.set_int32_ne serialized_value 0 @@ elem;
       print_bytes serialized_value;
       print_newline ();
       (pos, serialized_value)
    | _ -> failwith "NOT IMPLEMENTED"

  let convert_row (schema: Schema.t) (table_name: string) (row: row) =
    match Schema.Logical_Map.find_opt table_name schema with
    | Some (Table (table_info, _)) ->
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
    match Schema.Logical_Map.find_opt table_name schema with
    | Some (Table (_, row_id)) ->
       match Schema.calculate_size schema table_name with
       | None -> ();
       | Some size ->
          let serialized_row = convert_row schema table_name row in
          let out_channel = open_out_bin (Printf.sprintf "/tmp/%s.ndf" table_name) in
          seek_out out_channel ((Int32.mul size row_id) |> Int32.to_int);
          output_bytes out_channel serialized_row;
          close_out out_channel;
    | Some _
    | None -> failwith (Format.sprintf "Could not find table %s in the schema" table_name)
  
end

module Read = struct
  open Ast
  open Type
  open Entity

  type column_map = Ast.value StringMap.t
  
  let deserialize (schema: Schema.t) (table_name: string) (stream: bytes): column_map =
    let (Some (Table (table_info, _))) = Schema.Logical_Map.find_opt table_name schema in
    let columns  =
      Table_Info.to_seq table_info
      |> List.of_seq
      |> List.sort (fun (_, p1) (_, p2) -> compare p1.position p2.position)
      |> List.map (fun (name, {type'=type'; _}) -> (type', name))
    in
    
    let rec reconstruct_columns acc ((type', name)::types_rest) stream =
      let size = Type.to_byte_size type' |> Int32.to_int in
      let (name, literal) = (name, from_bytes type' (Bytes.sub stream 0 size)) in
      let ret = (name, literal) :: acc in
      if types_rest <> [] then
        reconstruct_columns ret types_rest (Bytes.sub stream size (Bytes.length stream - size))
      else
        ret
    in reconstruct_columns [] columns stream |> List.to_seq |> StringMap.of_seq  
end
