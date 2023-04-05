%{
  open Ast
%}

%token <int> INT
%token <string> STRING
%token <string> IDENT
%token LEFT_PAREN
%token RIGHT_PAREN
%token INSERT
%token VALUES
%token COMMA
%token SEMICOLON
%token EOF

%start <Ast.statement option> program
%%

program:
  | EOF
    { None }
  | s = statement; EOF
    { Some s }
  ;

statement:
  | INSERT; relation_name = IDENT; attribute_names = option(attribute_fields); VALUES; LEFT_PAREN; values = separated_nonempty_list(COMMA, value); RIGHT_PAREN; SEMICOLON
    {
      Ast.Insert { relation_name; attribute_names; values }
    }
  ;

attribute_fields: LEFT_PAREN; fs = separated_nonempty_list(COMMA, IDENT); RIGHT_PAREN
    {
      fs
    }
  ;

value:
  | i = INT
    { Ast.VInteger i}
  | s = STRING
    { Ast.VString s}
  ;
