
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
    | TString {size = size} -> Printf.sprintf "STRING:%ld" size
  let deserialize (candidate: string): t =
    match candidate with
    | "INTEGER" -> TInteger32
    | other when (String.sub other 0 6 = "STRING") ->
       String.split_on_char ':' other
           |> List.rev
           |> List.hd
           |> int_of_string
           |> fun size -> TString { size = Int32.of_int size }
    | _ -> failwith "Unknown string to deserialize"
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
    let lines = read_file filename in
    lines
  let parse_schema (lines: string list): t =
    let groups = split_groups "" lines in
    let logical_map = ref Logical_Map.empty in
    let inner (line: string) =
      let fields = String.split_on_char ' ' line |> Array.of_list in
      if fields.(0) = "TABLE" then
        logical_map :=
          !logical_map
           |> Logical_Map.add fields.(1) (Entity.Table_Info.empty, fields.(2) |> int_of_string)
      else
        let initial_entity =
          Entity.Table_Info.empty
          |> Entity.Table_Info.add fields.(0) ({position = (fields.(1) |> int_of_string); type' = Type.TString {size = 10l}}: Entity.field_metadata)
        in
        logical_map :=
          !logical_map
          |> Logical_Map.update
  
        (* ["TABLE Person 134", *)
        (*  "Age 1 INTEGER32", *)
        (*  "Name 0 STRING(10)"], *)

(*
  A fold that the fold function that picks an element and an accumulator and we check if the acc is an empty list and
  in that case we just spit a list of lists with x inside, otherwise you spit the head and tail and you check if the
  head is equal to the separator and then 
 *)  
    
    
end

type statement =
  | CreateRelation of {relation_name: string; attributes: (string * Type.t) list}
  | Insert of {relation_name: string; attributes: (string * Type.t * value) list}
  | Projection of {relation_name: string; attributes: string list}
  [@@deriving show]
  
