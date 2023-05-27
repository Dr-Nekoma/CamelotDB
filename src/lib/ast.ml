open Protocol_conv_xmlm
(* open Protocol_conv_json *)
(*
CREATE RELATION Person
  ID INTEGER
  Name STRING(20)
END RELATION
 *)


type value =
  | VInteger of int32
  | VString of string
  [@@deriving show]

module Type = struct
  type primitive = TInteger32
                 | TString of { size: int32 }
  [@@deriving show, protocol ~driver:(module Xmlm)]
  let to_byte_size (type': primitive) =
    match type' with
    | TInteger32 -> Int32.of_int(4)
    | TString { size } -> size
  let from_bytes (expected: primitive) (stream: bytes) =
    let received_size = Bytes.length stream |> Int32.of_int in
    match expected with
    | TInteger32 ->
       if received_size = 4l then
         VInteger (Bytes.get_int32_ne stream 0)
       else failwith (Format.sprintf "Int32 expects bytes of size 4, got: %ld" received_size)
    | TString {size} ->
       if received_size = size then
         VString (Bytes.to_string stream)
       else failwith (Format.sprintf "String(%ld) received the wrong size: %ld" size received_size)
  let non_param_from_string (candidate: string) : primitive =
    match candidate with
    | "INTEGER" -> TInteger32
    | "STRING" -> failwith "Strings are parametric types!"
    | g -> failwith (Format.sprintf "%s is not a type!" g)
  let param_from_string (candidate: string) (parameter: int32) : primitive =
    match candidate with
    | "STRING" -> TString { size = parameter }
    | "INTEGER" -> failwith "Integers are non-parametric types!"
    | g -> failwith (Format.sprintf "%s is not a type!" g)
end

module Entity = struct
  module Table_Info = Map.Make(String)
  type field_metadata =
    { position : int; type': Type.primitive }
  [@@deriving show, protocol ~driver:(module Xmlm)]
  type primitive =
    | Table of field_metadata Table_Info.t * int32
    | Procedure
end

(*
Example of a Schema
[
["TABLE Abc 134",
 "Age 1 INTEGER32",
 "Name 0 STRING 10"],
[""]   
["TABLE Person 0",
 "Age 1 INTEGER32",
 "Name 0 STRING 10"]
],
[""]
 *)

let split_groups (separator: string) (lines: string list): (string list) list =
  let inner (elem: string) (acc: (string list) list): (string list) list =
    if elem = separator then
      [] :: acc
    else
      match acc with
      | [] -> failwith "Something went terribly wrong xD"
      | (first::tail) -> (elem :: first) :: tail
  in List.fold_right inner lines [[]] |> List.filter ((<>) [])

module Schema = struct
  module Logical_Map = Map.Make(String)
  type t = Entity.primitive Logical_Map.t
  let calculate_size (schema: t) (table_name: string) : int32 option =
    match Logical_Map.find_opt table_name schema with
    | Some (Table (table_info, _)) ->
       Some (Logical_Map.fold (fun _ ({type'; _}: Entity.field_metadata) (acc: int32) -> Int32.add acc @@ Type.to_byte_size type') table_info 0l)
    | Some _
    | None -> None
  let serialize (schema: t) : Xmlm.t =
    let list_map = Logical_Map.to_seq schema |> List.of_seq in
    let serialized_list =
      List.map
        (fun (name, content) ->
          match content with
          | Entity.Table (field_metadata, rowId) ->
             let column_map = Entity.Table_Info.to_seq field_metadata |> List.of_seq in
             let serialized_list = List.map
               (fun (column_name, metadata) -> (column_name, Entity.field_metadata_to_xmlm metadata))
               column_map in
             (name, serialized_list, rowId)
        )
        list_map
end

type statement =
  | CreateRelation of {relation_name: string; attributes: (string * Type.primitive) list}
  | Insert of {relation_name: string; attributes: (string * Type.primitive * value) list}
  | Projection of {relation_name: string; attributes: string list}
  [@@deriving show]
  
