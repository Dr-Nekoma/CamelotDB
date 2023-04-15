
(*
CREATE RELATION Person
  ID INTEGER
  Name STRING(20)
END RELATION
*)

module Type = struct
  type t =
  | TInteger32
  | TString of { size: int }
  let to_byte_size (type': t) =
    match type' with
    | TInteger32 -> 4
    | TString { size } -> size
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

type value =
  | VInteger of int
  | VString of string
  [@@deriving show]

type statement =
  | Insert of {relation_name: string; attribute_names: string list option; values: value list}
  [@@deriving show]
  
