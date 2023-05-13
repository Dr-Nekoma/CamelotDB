{
open Parser

exception SyntaxError of string
}

let newline = '\r' | '\n' | "\r\n"
let white = [' ' '\t']+ | newline

let digit = ['0'-'9']
let lower_letter = ['a'-'z']
let upper_letter = ['A'-'Z']
let letter = lower_letter | upper_letter
let non_param_type' = "INTEGER"
let param_type' = "STRING"
let ident = letter (letter | '_')*
let upper_ident = upper_letter (letter '_')*
let int = '-'? ['0'-'9'] ['0'-'9']*

rule read =
  parse
  | "INSERT"    { INSERT }
  | "CREATE"    { CREATE }
  | "RELATION"  { RELATION }
  | "BEGIN"  { BEGIN }
  | "END"  { END }    
  | "BATCH"  { BATCH }
  | "PROJECT"  { PROJECT }  
  | "ON"  { ON }
  | non_param_type' { NON_PARAM_TYPE (Lexing.lexeme lexbuf) }
  | param_type' { PARAM_TYPE (Lexing.lexeme lexbuf) }  
  | ident { IDENT (Lexing.lexeme lexbuf) }  
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | white { read lexbuf }
  | newline { read lexbuf }
  | int { LITERAL_INT (Int32.(of_string (Lexing.lexeme lexbuf))) }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_string buf =
  parse
  | '"'       { LITERAL_STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (SyntaxError ("String is not terminated")) }
