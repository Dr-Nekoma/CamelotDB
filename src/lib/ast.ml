type value =
  | VInteger of int
  | VString of string
  [@@deriving show]

type statement =
  | Insert of {table_name: string; columns_names: string list option; values: value list}
  [@@deriving show]
  
