
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
  | TString of { size: int }
  let to_byte_size (type': t) =
    match type' with
    | TInteger32 -> 4
    | TString { size } -> size
  let from_bytes (expected: t) (stream: bytes) =
    let received_size = Bytes.length stream in
    match expected with
    | TInteger32 ->
       if received_size = 4 then
         VInteger (Bytes.get_int32_ne stream 0)
       else failwith (Format.sprintf "Int32 expects bytes of size 4, got: %d" received_size)
    | TString {size} ->
       if received_size = size then
         VString (Bytes.to_string stream)
       else failwith (Format.sprintf "String(%d) received the wrong size: %d" size received_size)
end

module Entity = struct
  module Table_Info = Map.Make(String)
  type field_metadata = { position : int; type': Type.t }
  type t =
    | Table of field_metadata Table_Info.t
    | Procedure
end

module Schema = struct
  module Logical_Map = Map.Make(String)
  type t = Entity.t Logical_Map.t
end

type statement =
  | Insert of {relation_name: string; attribute_names: string list option; values: value list}
  [@@deriving show]
  
