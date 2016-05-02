open Ast

module String = BatString

type handle = int
type code = string
type value =
  | Val_null
  | Val_int of int
  | Val_bool of bool
  | Val_real of float
  | Val_string of string
  | Val_handle of handle
  | Val_code of code

let s_value = function
  | Val_int v -> string_of_int v
  | Val_bool v -> string_of_bool v
  | Val_real v -> string_of_float v
  | Val_string v -> String.quote v
  | Val_null -> "null"
  | Val_code v -> "function " ^ v
  | Val_handle v -> "handle " ^ (string_of_int v)

type env = {
  var_table: (string, value ref) Hashtbl.t;
  array_table: (string, (int, value ref) Hashtbl.t) Hashtbl.t;
}

type context = {
  type_decls: (string, loc * type_desc) Hashtbl.t;
  global_decls: (string, loc * var_desc) Hashtbl.t;
  func_decls: (string, loc * func_desc) Hashtbl.t;
  globals: env;
  mutable stack: env list;
}

exception Return
exception Exitwhen

let new_env () =
  { var_table = Hashtbl.create 0;
    array_table = Hashtbl.create 0; }

let lookup ctx id =
  let lookup_env env id =
    Hashtbl.find env.var_table id
  in
  try lookup_env (List.hd ctx.stack) id with
  | _ -> lookup_env ctx.globals id

let create () =
  { type_decls = Hashtbl.create 0;
    global_decls = Hashtbl.create 0;
    func_decls = Hashtbl.create 0;
    globals = new_env ();
    stack = []; }

let rec eval ctx (loc, expr) =
  match expr with
  | Exp_literal (Lit_int v) -> Val_int v
  | Exp_literal (Lit_real v) -> Val_real v
  | Exp_literal (Lit_bool v) -> Val_bool v
  | Exp_literal (Lit_string v) -> Val_string v
  | Exp_literal Lit_null -> Val_null
  | Exp_ident (_, id) -> !(lookup ctx id)
  | Exp_funcref (_, id) -> Val_code id
  | Exp_subscript ((_, id), e) -> (
    let a = Hashtbl.find ctx.globals.array_table id in
    let v = eval ctx e in
    match v with
    | Val_int v -> !(Hashtbl.find a v)
    | _ -> assert false
    )
  | Exp_call ((_, id), args) -> (
    let args = List.map (eval ctx) args in
    call ctx id args
    )
  | Exp_unary (op, e) -> (
    let v = eval ctx e in
    match op, v with
    | Uop_not, (Val_bool v) -> Val_bool (not v)
    | Uop_neg, (Val_int v) -> Val_int (-v)
    | Uop_neg, (Val_real v) -> Val_real (-.v)
    | Uop_pos, (Val_int _) | Uop_pos, (Val_real _) -> v
    | _ -> assert false
    )
  | Exp_binary (op, e1, e2) -> (
    let v1 = eval ctx e1 in
    let v2 = eval ctx e2 in
    match op, v1, v2 with
    (* int *)
    | Bop_plus, (Val_int v1), (Val_int v2) -> Val_int (v1 + v2)
    | Bop_minus, (Val_int v1), (Val_int v2) -> Val_int (v1 - v2)
    | Bop_mult, (Val_int v1), (Val_int v2) -> Val_int (v1 * v2)
    | Bop_div, (Val_int v1), (Val_int v2) -> Val_int (v1 / v2)
    | Bop_eqeq, (Val_int v1), (Val_int v2) -> Val_bool (v1 = v2)
    | Bop_neq, (Val_int v1), (Val_int v2) -> Val_bool (v1 <> v2)
    | Bop_lt, (Val_int v1), (Val_int v2) -> Val_bool (v1 < v2)
    | Bop_gt, (Val_int v1), (Val_int v2) -> Val_bool (v1 > v2)
    | Bop_leq, (Val_int v1), (Val_int v2) -> Val_bool (v1 <= v2)
    | Bop_geq, (Val_int v1), (Val_int v2) -> Val_bool (v1 >= v2)
    (* real *)
    | Bop_plus, (Val_real v1), (Val_real v2) -> Val_real (v1 +. v2)
    | Bop_minus, (Val_real v1), (Val_real v2) -> Val_real (v1 -. v2)
    | Bop_mult, (Val_real v1), (Val_real v2) -> Val_real (v1 *. v2)
    | Bop_div, (Val_real v1), (Val_real v2) -> Val_real (v1 /. v2)
    | Bop_eqeq, (Val_real v1), (Val_real v2) -> Val_bool (v1 = v2)
    | Bop_neq, (Val_real v1), (Val_real v2) -> Val_bool (v1 <> v2)
    | Bop_lt, (Val_real v1), (Val_real v2) -> Val_bool (v1 < v2)
    | Bop_gt, (Val_real v1), (Val_real v2) -> Val_bool (v1 > v2)
    | Bop_leq, (Val_real v1), (Val_real v2) -> Val_bool (v1 <= v2)
    | Bop_geq, (Val_real v1), (Val_real v2) -> Val_bool (v1 >= v2)
    (* bool *)
    | Bop_and, (Val_bool v1), (Val_bool v2) -> Val_bool (v1 && v2)
    | Bop_or, (Val_bool v1), (Val_bool v2) -> Val_bool (v1 || v2)
    | _ -> assert false
    )

and init_vars ctx env vars =
  List.iter (fun (loc, decl) ->
    let _, name = decl.v_name in
    if decl.v_array then
      Hashtbl.replace env.array_table name (Hashtbl.create 0)
    else
      let value = (
        match decl.v_init with
        | Some init -> eval ctx init
        | None -> Val_null
      ) in
      Hashtbl.replace env.var_table name (ref value)
  ) vars

and call ctx func_id args =
  let (_, func_decl) = Hashtbl.find ctx.func_decls func_id in
  let locals = new_env () in
  let args = List.combine func_decl.fn_params args in
  List.iter (
    fun ((_, (_, p)), v) ->
    Hashtbl.replace locals.var_table p (ref v)
  ) args;
  ctx.stack <- locals :: ctx.stack;
  init_vars ctx locals func_decl.fn_locals;
  let retval = ref Val_null in
  let rec interp = function
    | (loc, stmt) :: rest -> (
      ( match stmt with
        | Stmt_return (Some e) -> (
          retval := eval ctx e;
          raise Return
          )
        | Stmt_return None -> raise Return
        | Stmt_exitwhen e -> (
          let v = eval ctx e in
          match v with
          | Val_bool true -> raise Exitwhen
          | Val_bool false -> ()
          | _ -> assert false
        )
        | Stmt_set ((_, id), None, e) -> (
          let refv = lookup ctx id in
          refv := eval ctx e
        )
        | Stmt_call c -> ignore (eval ctx (loc, Exp_call c))
        | Stmt_loop block -> (
          try
            while true do
              interp block
            done
          with Exitwhen -> ()
        )
        | Stmt_if (ifthens, else_) -> (
          let rec interp_if = function
            | [] -> false
            | (cond, then_) :: rest -> (
              let cond = eval ctx cond in
              ( match cond with
                | Val_bool true -> interp then_
                | Val_bool false -> ()
                | _ -> assert false
              );
              interp_if rest
            )
          in
          let interp_else = not (interp_if ifthens) in
          match else_ with
          | Some block -> if interp_else then interp block
          | None -> ()
        )
        | _ -> assert false
      );
      interp rest
    )
    | [] -> ()
  in
  (try interp func_decl.fn_body with Return -> ());
  ctx.stack <- List.tl ctx.stack;
  !retval

let load ctx file =
  let lexbuf = Lexing.from_channel file in
  let file = Parser.file Lexer.token lexbuf in
  List.iter (fun (loc, decl) ->
    match decl with
    | Decl_type (this, base) ->
      let _, name = this in
      Hashtbl.replace ctx.type_decls name (loc, (this, base))
    | Decl_func decl ->
      let _, name = decl.fn_name in
      Hashtbl.replace ctx.func_decls name (loc, decl)
    | Decl_globals globals -> (
      List.iter (
        fun (loc, decl) ->
        let _, name = decl.v_name in
        Hashtbl.replace ctx.global_decls name (loc, decl)
        ) globals;
      init_vars ctx ctx.globals globals
    )
  ) file
