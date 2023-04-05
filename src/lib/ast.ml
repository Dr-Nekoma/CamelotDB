type value =
  | VInteger of int
  | VString of string
  [@@deriving show]

type statement =
  | Insert of {relation_name: string; attribute_names: string list option; values: value list}
  [@@deriving show]
  
