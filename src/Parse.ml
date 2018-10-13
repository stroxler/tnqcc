open Base
open Ast

module T = Tok


let emsg (context: string) (tokens: T.t list) =
  let _, lineno = List.hd_exn tokens in
  let tokens_str = T.show_tokens @@ List.take tokens 10 in
  Printf.sprintf "Parse error, line %d:\nError context: %s\ntokens: %s"
                 lineno context tokens_str


let rec parse_expr tokens = match tokens with
  (* unary operators *)
  | (T.OP_MINUS, _) :: more ->
    let inner, rest = parse_expr more in
    UnaryOp (Neg, inner), rest
  | (T.OP_BANG, _) :: more ->
    let inner, rest = parse_expr more in
    UnaryOp (LNot, inner), rest
  | (T.OP_TILDE, _) :: more ->
    let inner, rest = parse_expr more in
    UnaryOp (BNot, inner), rest
  (* literals *)
  | (T.LIT_INT n, _) :: rest ->
    Lit (Int n), rest
  (* parse failure *)
  | bad_tokens -> failwith @@ emsg "parse_expr" bad_tokens



let parse_block_item tokens = match tokens with
  | (T.KW_INT, lineno) :: (T.ID varname, _) :: (T.SEMICOL, _) :: rest ->
    let def_var = DefVar {annot=IntAnnot; name=Id varname; init=None} in
    Definition (def_var, Line lineno), rest
  (* TODO add definition-with-initializer match *)
  | (T.KW_RETURN, lineno) :: more_tokens ->
    let value, after_exp_tokens = parse_expr more_tokens in
    (match after_exp_tokens with
     | (T.SEMICOL, _) :: rest ->
       Statement (Return value, Line lineno), rest
     | _ -> failwith @@ emsg "parse_block_item -> return" after_exp_tokens
    )
  | bad_tokens -> failwith @@ emsg "parse_block_item" bad_tokens


let parse_block (block_tokens: T.t list) =
  let rec go tokens block_items =
    match tokens with
    | (T.RBRACE, _) :: rest ->
      (List.rev block_items, rest)
    | _ ->
      let next_item, rest = parse_block_item tokens in
      go rest (next_item::block_items)
  in go block_tokens []


let parse_fn (fn_name: string) (fn_tokens: T.t list) (lineno: int) =
  let parse_fn_arg_list = function
    | (T.RPAREN, _) :: rest -> (), rest
    | bad_tokens -> failwith @@ emsg "parse_fn_arg_list" bad_tokens
  in
  let parse_fn_body = function
    | (T.SEMICOL, _) :: rest ->
      (None, rest)
    | (T.LBRACE, _) :: more ->
      let block, rest = parse_block more in
      (Some block, rest)
    | bad_tokens -> failwith @@ emsg "parse_fn_body" bad_tokens
  in
  let _, after_args_tokens = parse_fn_arg_list fn_tokens in
  let body, rest = parse_fn_body after_args_tokens in
  let fn = DefFn {
      annot  = IntAnnot;
      name   = (Id fn_name);
      body   = body;
      line   = Line lineno;
    } in
   (fn, rest)


let rec parse_fn_defs (tokens: T.t list) (parsed_fns: def_fn list) =
  match tokens with
  | (T.EOF, _) :: _ -> List.rev parsed_fns
  | (T.KW_INT, lineno) :: (T.ID name, _) :: (T.LPAREN, _) :: more_tokens ->
    let fn, rest = parse_fn name more_tokens lineno in
    parse_fn_defs rest (fn::parsed_fns)
  | _ -> failwith @@ emsg "parse_fn_defs" tokens


let parse_prog (tokens: T.t list) =
  let parsed = parse_fn_defs tokens [] in
  Prog parsed


let parse = parse_prog
