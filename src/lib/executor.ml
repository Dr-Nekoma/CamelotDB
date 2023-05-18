open Ast

let execute (schema: Schema.t) (statement: statement) : Schema.t =
  match statement with
  | Insert { relation_name; attributes} ->
     match Schema.Logical_Map.find_opt relation_name schema with
     | Some (Table (_, _)) ->
        let row = List.map
                    (* This is not doing type checking xD *)
                    (fun (name, _, value) -> (name, value)) attributes
                  |> List.to_seq
                  |> Io.StringMap.of_seq
        in
        Io.Write.write_row_on_disk schema relation_name row;
        Schema.Logical_Map.update relation_name
          (Option.map (function Entity.Table (fields, row_id) -> Entity.Table (fields, Int32.add row_id 1l))) schema
     | _ -> failwith "something"
  | _ -> failwith "something 2"