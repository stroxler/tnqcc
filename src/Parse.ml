open Base
open Ast

module T = Tok


let emsg (context: string) (tokens: T.t list) =
  let tokens_str = T.show_tokens @@ List.take tokens 20 in
  Printf.sprintf "Parse error (in %s), at %s" context tokens_str


let parse_expr = function
  | T.LIT_INT n :: rest ->
    (Lit (Int n), rest)
  | bad_tokens -> failwith @@ emsg "parse_expr" bad_tokens



let parse_block_item = function
  | T.KW_INT :: T.ID varname :: T.SEMICOL :: rest ->
    let def_var = DefVar {annot=IntAnnot; name=Id varname; init=None} in
    (Definition def_var, rest)
  (* TODO add definition-with-initializer match *)
  | T.KW_RETURN :: more_tokens ->
    let value, after_exp_tokens = parse_expr more_tokens in
    (match after_exp_tokens with
     | T.SEMICOL :: rest ->
       (Statement (Return value), rest)
     | _ -> failwith @@ emsg "parse_block_item -> return" after_exp_tokens
    )
  | bad_tokens -> failwith @@ emsg "parse_block_item" bad_tokens


let parse_block (block_tokens: T.t list) =
  let rec go tokens block_items =
    match tokens with
    | T.RBRACE :: rest ->
      (List.rev block_items, rest)
    | _ ->
      let next_item, rest = parse_block_item tokens in
      go rest (next_item::block_items)
  in go block_tokens []


let parse_fn (fn_name: string) (fn_tokens: T.t list) =
  let parse_fn_arg_list = function
    | T.RPAREN :: rest -> ((), rest)
    | bad_tokens -> failwith @@ emsg "parse_fn_arg_list" bad_tokens
  in
  let parse_fn_body = function
    | T.SEMICOL :: rest ->
      (None, rest)
    | T.LBRACE :: more ->
      let block, rest = parse_block more in
      (Some block, rest)
    | bad_tokens -> failwith @@ emsg "parse_fn_body" bad_tokens
  in
  let _, after_args_tokens = parse_fn_arg_list fn_tokens in
  let body, rest = parse_fn_body after_args_tokens in
  let fn = DefFn {
      annot = IntAnnot;
      name  = (Id fn_name);
      body  = body
    } in
   (fn, rest)


let parse tokens_list =
  let rec go (tokens: T.t list) (parsed_fns: def_fn list) =
    match tokens with
    | [] -> List.rev parsed_fns
    | T.KW_INT :: T.ID name :: T.LPAREN :: more_tokens ->
      let fn, rest = parse_fn name more_tokens in
      go rest (fn::parsed_fns)
    | _ -> failwith @@ emsg "parse" tokens
  in
  let parsed = go tokens_list [] in
  Prog parsed

