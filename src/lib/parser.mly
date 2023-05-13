%{
  open Ast
%}

%token <int32> LITERAL_INT
%token <string> LITERAL_STRING
%token <string> PARAM_TYPE
%token <string> NON_PARAM_TYPE
%token <string> IDENT
%token INSERT
%token CREATE
%token RELATION
%token LEFT_PAREN
%token RIGHT_PAREN
%token COMMA
%token SEMICOLON
%token BATCH
%token PROJECT
%token ON
%token BEGIN
%token END
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
  | INSERT; relation_name = IDENT; LEFT_PAREN; attributes = separated_nonempty_list(COMMA, group_insert); RIGHT_PAREN
    {
      Ast.Insert { relation_name; attributes }
    }
  | CREATE; RELATION; relation_name = IDENT; LEFT_PAREN; attributes = separated_nonempty_list(COMMA, group_create); RIGHT_PAREN
    {
      Ast.CreateRelation { relation_name; attributes }
    }
  | PROJECT; attributes = separated_nonempty_list(COMMA, IDENT); ON; relation_name = IDENT
    {
      Ast.Projection { relation_name; attributes }
    }

  ;

type_parser:
  | typey = NON_PARAM_TYPE { Ast.Type.non_param_from_string typey }
  | typey = PARAM_TYPE; LEFT_PAREN; parameter = LITERAL_INT; RIGHT_PAREN
    {
      Ast.Type.param_from_string typey parameter
    }
  ;  

group_insert:
  | name = IDENT; typey = type_parser; valuey = value
    {
      (name, typey, valuey)
    }
  ;

group_create: name = IDENT; typey = type_parser
    {
      (name, typey)
    }
  ;

value:
  | i = LITERAL_INT
    { Ast.VInteger i}
  | s = LITERAL_STRING
    { Ast.VString s}
  ;
