{
open Parser

exception SyntaxError of string
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let digit = ['0'-'9']
let lower_letter = ['a'-'z']
let upper_letter = ['A'-'Z']
let letter = lower_letter | upper_letter

let ident = letter (letter | '_')*
let upper_ident = upper_letter (letter '_')*
let int = '-'? ['0'-'9'] ['0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | newline { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | "INSERT" { INSERT }
  | "VALUES" { VALUES }
  | ',' { COMMA }
  | ';' { SEMICOLON }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | ident { IDENT (Lexing.lexeme lexbuf) }  
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
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
