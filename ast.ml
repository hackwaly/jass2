type loc = Lexing.position * Lexing.position

and ident = loc * string

and file = decl list

and decl = loc * decl_desc

and decl_desc =
  | Decl_type of type_desc
  | Decl_globals of var_decl list
  | Decl_func of func_desc

and type_desc = ident * ident

and var_decl = loc * var_desc
and var_desc = {
  v_const: bool;
  v_array: bool;
  v_type: ident;
  v_name: ident;
  v_init: expr option;
}

and func_desc = {
  fn_const: bool;
  fn_native: bool;
  fn_name: ident;
  fn_params: (ident * ident) list;
  fn_type: ident option;
  fn_locals: var_decl list;
  fn_body: statement list;
}

and statement = loc * statement_desc

and statement_desc =
  | Stmt_set of ident * expr option * expr
  | Stmt_call of call_desc
  | Stmt_if of ifthen_desc list * statement list option
  | Stmt_loop of statement list
  | Stmt_exitwhen of expr
  | Stmt_return of expr option

and ifthen_desc = expr * (statement list)

and expr = loc * expr_desc
and expr_desc =
  | Exp_literal of literal
  | Exp_ident of ident
  | Exp_unary of unary_op * expr
  | Exp_binary of binary_op * expr * expr
  | Exp_subscript of subscript_desc
  | Exp_call of call_desc
  | Exp_funcref of ident

and subscript_desc = ident * expr

and call_desc = ident * (expr list)

and literal =
  | Lit_int of int
  | Lit_real of float
  | Lit_bool of bool
  | Lit_string of string
  | Lit_null

and unary_op =
  | Uop_pos
  | Uop_neg
  | Uop_not

and binary_op =
  | Bop_plus
  | Bop_minus
  | Bop_mult
  | Bop_div
  | Bop_eqeq
  | Bop_neq
  | Bop_lt
  | Bop_gt
  | Bop_geq
  | Bop_leq
  | Bop_and
  | Bop_or
