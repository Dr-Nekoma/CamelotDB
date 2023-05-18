
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
  type t =
  | TInteger32
  | TString of { size: int32 }
  [@@deriving show]
  let to_byte_size (type': t) =
    match type' with
    | TInteger32 -> Int32.of_int(4)
    | TString { size } -> size
  let from_bytes (expected: t) (stream: bytes) =
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
  let non_param_from_string (candidate: string) : t =
    match candidate with
    | "INTEGER" -> TInteger32
    | "STRING" -> failwith "Strings are parametric types!"
    | g -> failwith (Format.sprintf "%s is not a type!" g)
  let param_from_string (candidate: string) (parameter: int32) : t =
    match candidate with
    | "STRING" -> TString { size = parameter }
    | "INTEGER" -> failwith "Integers are non-parametric types!"
    | g -> failwith (Format.sprintf "%s is not a type!" g)
  let serialize (type': t) =
    match type' with
    | TInteger32 -> "INTEGER32"
    | TString {size = size} -> Printf.sprintf "STRING %ld" size
end

module Entity = struct
  module Table_Info = Map.Make(String)
  type field_metadata = { position : int; type': Type.t }
  type t =
    | Table of field_metadata Table_Info.t * int32
    | Procedure
  let serialize (entity: t) =
    match entity with
    | Table (table_info, row_id) ->
       let initial = Printf.sprintf "%ld\n" row_id in
       Table_Info.fold (fun name { position = position; type' = type' } acc ->
         Printf.sprintf "%s%s %d %s\n" acc name position (Type.serialize type')) table_info initial
    | Procedure -> ""
end

module Schema = struct
  module Logical_Map = Map.Make(String)
  type t = Entity.t Logical_Map.t
  let calculate_size (schema: t) (table_name: string) : int32 option =
    match Logical_Map.find_opt table_name schema with
    | Some (Table (table_info, _)) ->
       Some (Logical_Map.fold (fun _ ({type'; _}: Entity.field_metadata) (acc: int32) -> Int32.add acc @@ Type.to_byte_size type') table_info 0l)
    | Some _
    | None -> None
  let serialize (schema: t) =
    Logical_Map.fold (fun name entity acc ->
        Printf.sprintf "%sTABLE %s %s\n" acc name (Entity.serialize entity)) schema ""
  let deserialize (filename: string) =
    print_endline filename;
    let read_file filename = 
      let lines = ref [] in
      let chan = open_in filename in
      try
        while true; do
          lines := input_line chan :: !lines
        done; !lines
      with End_of_file ->
        close_in chan;
        List.rev !lines in
    List.iter (fun x -> print_endline x;) (read_file filename)
end

type statement =
  | CreateRelation of {relation_name: string; attributes: (string * Type.t) list}
  | Insert of {relation_name: string; attributes: (string * Type.t * value) list}
  | Projection of {relation_name: string; attributes: string list}
  [@@deriving show]
  
