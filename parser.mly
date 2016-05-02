%{
open Ast

module Option = BatOption

type expr_ident_follow =
  | Eif_args of expr list
  | Eif_subscript of expr
  | Eif_none


let s_token = function
  | AND -> "AND"
  | ARRAY -> "ARRAY"
  | BOOLEAN -> "BOOLEAN"
  | CALL -> "CALL"
  | CODE -> "CODE"
  | COMMA -> "COMMA"
  | CONSTANT -> "CONSTANT"
  | DIV -> "DIV"
  | ELSE -> "ELSE"
  | ELSEIF -> "ELSEIF"
  | ENDFUNCTION -> "ENDFUNCTION"
  | ENDGLOBALS -> "ENDGLOBALS"
  | ENDIF -> "ENDIF"
  | ENDLOOP -> "ENDLOOP"
  | EOF -> "EOF"
  | EQ -> "EQ"
  | EQEQ -> "EQEQ"
  | EXITWHEN -> "EXITWHEN"
  | EXTENDS -> "EXTENDS"
  | FALSE -> "FALSE"
  | FUNCTION -> "FUNCTION"
  | GEQ -> "GEQ"
  | GLOBALS -> "GLOBALS"
  | GT -> "GT"
  | HANDLE -> "HANDLE"
  | IF -> "IF"
  | INTEGER -> "INTEGER"
  | LBRACKET -> "LBRACKET"
  | LEQ -> "LEQ"
  | LOCAL -> "LOCAL"
  | LOOP -> "LOOP"
  | LPAREN -> "LPAREN"
  | LT -> "LT"
  | MINUS -> "MINUS"
  | MULT -> "MULT"
  | NATIVE -> "NATIVE"
  | NEQ -> "NEQ"
  | NL -> "NL"
  | NOT -> "NOT"
  | NOTHING -> "NOTHING"
  | NULL -> "NULL"
  | OR -> "OR"
  | PLUS -> "PLUS"
  | RBRACKET -> "RBRACKET"
  | REAL -> "REAL"
  | RETURN -> "RETURN"
  | RETURNS -> "RETURNS"
  | RPAREN -> "RPAREN"
  | SET -> "SET"
  | STRING -> "STRING"
  | TAKES -> "TAKES"
  | THEN -> "THEN"
  | TRUE -> "TRUE"
  | TYPE -> "TYPE"
  | INT_LITERAL _ -> "INT_LITERAL"
  | REAL_LITERAL _ -> "REAL_LITERAL"
  | STRING_LITERAL _ -> "STRING_LITERAL"
  | IDENT _ -> "IDENT"

%}

%token AND ARRAY BOOLEAN CALL CODE COMMA CONSTANT DIV ELSE ELSEIF ENDFUNCTION ENDGLOBALS ENDIF ENDLOOP EOF EQ EQEQ EXITWHEN EXTENDS FALSE FUNCTION GEQ GLOBALS GT HANDLE IF INTEGER LBRACKET LEQ LOCAL LOOP LPAREN LT MINUS MULT NATIVE NEQ NL NOT NOTHING NULL OR PLUS RBRACKET REAL RETURN RETURNS RPAREN SET STRING TAKES THEN TRUE TYPE

%token <int> INT_LITERAL
%token <float> REAL_LITERAL
%token <string> STRING_LITERAL IDENT

%left AND OR
%left LT GT EQEQ NEQ GEQ LEQ
%left PLUS MINUS
%left MULT DIV
%nonassoc UNARY

%start <Ast.file> file

%%

segment(p):
  | p
    { ($startpos, $endpos), $1 }
  ;

file:
  | nl? decls=segment(decl)* EOF
    { decls }
  ;

decl:
  | TYPE type_=ident EXTENDS base_type=base_type nl
    { Decl_type (type_, base_type) }
  | GLOBALS nl vars=global_var* ENDGLOBALS nl
    { Decl_globals vars }
  | const=CONSTANT? decl=func_decl
    { Decl_func { decl with
        fn_const = Option.is_some const;
      } }
  ;

func_decl:
  | NATIVE decl=func_sig nl
    { { decl with
        fn_native = true; } }
  | FUNCTION decl=func_sig nl
    locals=local_var* body=statement* ENDFUNCTION nl
    { { decl with
        fn_locals = locals;
        fn_body = body;
      } }

%inline base_type:
  | segment(_base_type)                 { $1 }
%inline _base_type:
  | HANDLE                              { "handle" }
  | IDENT                               { $1 }
  ;

global_var:
  | segment(_global_var)                { $1 }
_global_var:
  | CONSTANT type_=type_ name=ident EQ init=expr nl
    { { v_const = true;
        v_array = false;
        v_type = type_;
        v_name = name;
        v_init = Some init; } }
  | decl=var_decl nl
    { decl }
  ;

func_sig:
  | name=ident TAKES params=param_list RETURNS return_type=return_type
    { { fn_name = name;
        fn_params = params;
        fn_type = return_type;
        fn_const = false;
        fn_native = false;
        fn_locals = [];
        fn_body = []; } }
  ;

param_list:
  | NOTHING                             { [] }
  | separated_list(COMMA, param)        { $1 }
  ;

param:
  | type_=type_ ident=ident             { type_, ident }
  ;

return_type:
  | NOTHING                             { None }
  | type_                               { Some $1 }
  ;

local_var:
  | segment(_local_var)                 { $1 }
_local_var:
  | LOCAL decl=var_decl nl              { decl }
  ;

var_decl:
  | type_=type_ name=ident init=preceded(EQ, expr)?
    { { v_type = type_;
        v_name = name;
        v_const = false;
        v_array = false;
        v_init = init; } }
  | type_=type_ ARRAY name=ident
    { { v_type = type_;
        v_name = name;
        v_const = false;
        v_array = true;
        v_init = None; } }
  ;

statement:
  | segment(_statement)                 { $1 }
_statement:
  | SET var=ident sub=subscript? EQ value=expr nl
    { Stmt_set (var, sub, value) }
  | CALL func=ident args=args nl
    { Stmt_call (func, args) }
  | if_=if_ elseif=elseif* else_=else_? ENDIF nl
    { Stmt_if (if_ :: elseif, else_) }
  | LOOP nl block=statement* ENDLOOP nl
    { Stmt_loop block }
  | EXITWHEN expr=expr nl
    { Stmt_exitwhen expr }
  | RETURN expr=expr? nl
    { Stmt_return expr }
  ;

args:
  | delimited(LPAREN, separated_list(COMMA, expr), RPAREN)
    { $1 }
  ;

if_:
  | IF cond=expr THEN nl block=statement*
    { cond, block }
elseif:
  | ELSEIF cond=expr THEN nl block=statement*
    { cond, block }
  ;
else_:
  | ELSE nl block=statement*
    { block }
  ;

expr:
  | segment(_expr)                      { $1 }
_expr:
  | lhs=expr op=binary_op rhs=expr
    { Exp_binary (op, lhs, rhs) }
  | op=unary_op expr=expr
    { Exp_unary (op, expr) }
    %prec UNARY
  | id=ident eif=expr_ident_follow
    { match eif with
      | Eif_none -> Exp_ident id
      | Eif_args args -> Exp_call (id, args)
      | Eif_subscript expr -> Exp_subscript (id, expr) }
  | FUNCTION name=ident                 { Exp_funcref name }
  | literal
    { Exp_literal $1 }
  | LPAREN expr=_expr RPAREN            { expr }
  ;

expr_ident_follow:
  |                                     { Eif_none }
  | subscript                           { Eif_subscript $1 }
  | args                                { Eif_args $1 }

%inline binary_op:
  | PLUS                                { Bop_plus }
  | MINUS                               { Bop_minus }
  | MULT                                { Bop_mult }
  | DIV                                 { Bop_div }
  | LT                                  { Bop_lt }
  | GT                                  { Bop_gt }
  | EQEQ                                { Bop_eqeq }
  | NEQ                                 { Bop_neq }
  | GEQ                                 { Bop_geq }
  | LEQ                                 { Bop_leq }
  | AND                                 { Bop_and }
  | OR                                  { Bop_or }
  ;

%inline unary_op:
  | PLUS                                { Uop_pos }
  | MINUS                               { Uop_neg }
  | NOT                                 { Uop_not }
  ;

subscript:
  | delimited(LBRACKET, expr, RBRACKET) { $1 }
  ;

literal:
  | INT_LITERAL                         { Lit_int $1 }
  | REAL_LITERAL                        { Lit_real $1 }
  | STRING_LITERAL                      { Lit_string $1 }
  | TRUE                                { Lit_bool true }
  | FALSE                               { Lit_bool false }
  | NULL                                { Lit_null }
  ;

type_:
  | segment(_type)                      { $1 }

%inline _type:
  | IDENT                               { $1 }
  | CODE                                { "code" }
  | HANDLE                              { "handle" }
  | INTEGER                             { "integer" }
  | REAL                                { "real" }
  | BOOLEAN                             { "boolean" }
  | STRING                              { "string" }
  ;

ident:
  | segment(IDENT)                      { $1 }

nl:
  | NL+                                 {}
