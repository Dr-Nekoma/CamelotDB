module Write = struct
  open Ast

  exception InvalidEntityConvertion
  
  let convert_table (table: Entity.field_metadata Entity.Table_Info.t) (attribute_name: string) (entity_name: string) (literal: value) =
    match  with
      (* | Some (Entity.Table table) -> *)
         (* Entity.Table_Info.find_opt attribute_name table *)
      (* | Some (Entity.Procedure) *)
      (* | None -> raise InvalidEntityConvertion *)
end
