open Base
open Ast

module T = Tok

(* General references:
   https://www.cs.dartmouth.edu/~mckeeman/cs48/references/c.html
   https://www.lysator.liu.se/c/ANSI-C-grammar-y.html
   ...
   note that the binary operations are defined left-recursively,
   but the approach we are using (which is effectively right-recursive
   parsing due to the fact that our recursive descent parser cannot
   be left-recursive, but left-associative AST construction) is equivalent,
   for the subset of the grammar that we implement.
*)

let emsg (context: string) (tokens: T.t list) =
  let _, lineno = List.hd_exn tokens in
  let tokens_str = T.show_tokens @@ List.take tokens 10 in
  Printf.sprintf "Parse error, line %d:\nError context: %s\ntokens: %s"
    lineno context tokens_str

(* The hardest thing in C parsing is using recursive descent to parse
   binary expressions of left-associative operators. The approach we use here is
   to organize them by precedence, so that we parse expressions of operators
   with lower precedence as a bunch of operator-joined expressions of operators
   with higher precedence.

   The first function below, `parse_bin_expr_one_level`, encapsulates this idea
   by saying that we're going to parse expressions in terms of operators at some
   precedence level left-associatively, where each term is an expression
   composed of either higher-precedence operators or "other things" such as
   parenthesized expressions, unary operators, etc.

   Because in C almost all of the binary operators have precedence adjacent to
   one another:
    - all of them except assignment-related operations have precedence
      higher than the ternary operator
    - all of them except lookups (array indexing "[]" and struct lookups
      "." and "->" have precedence lower than any unary operator
   So except for assignment and lookups, we can automate most of the operator
   parsing by forming a lookup table by level.

   This is what `parse_bin_expr_by_prec` does: we create a `binary_op_by_prec`
   map from precedence to a map from token to operator, and this allows us
   to chain together a bunch of binary operator parsing levels by:
     - starting at low precedence
     - for our term parser, just use the next highest precedence
     - use some innermost parser (e.g. unary expressions) once we run out of
       precedences
   An added benefit is seeing how Base.Map works with user-defined types
   (note that we hadded to add a Tok.Token module)

   Note that I'm defining precedence so that low precedence means late-binding,
   e.g. + is lower than * ... some references list them the opposite way.

   Also note that the approach we use here is relatively easy to extend to
   user-defined operators with user-defined precedence (as in haskell), since we
   are storing the operators and precedences as data structures which could be
   made environment-dependent. User-defined associativity would be a bit more
   involved, but if `parse_bin_expr` built up a list of terms instead of
   creating the AST on the fly, then we could group them dynamically based on
   operator associativity rules.
*)


let mk_binary_ops_by_prec
    (ops_alists : ((T.token * binary_op) list) list) =
  let mk_binary_ops alist =
    Map.of_alist_exn (module T.Token) alist in
  let zipper i alist =
    i, (mk_binary_ops alist) in
  Map.of_alist_exn (module Int) @@ List.mapi ~f:zipper ops_alists


(* See https://en.cppreference.com/w/c/language/operator_precedence
   (which lists preferences in the opposite order) *)
let binary_ops_by_prec = mk_binary_ops_by_prec [
    (* assignments would go here *)
    [(T.OP_DOR,    LOr);];
    [(T.OP_DAND,  LAnd);];
    [(T.OP_SOR,    BOr);];
    [(T.OP_XOR,    XOr);];
    [(T.OP_SAND,  BAnd);];
    [
      (T.OP_DEQ,   Eq);
      (T.OP_NEQ,  Neq);
    ];
    [
      (T.OP_LEQ,   Leq);
      (T.OP_GEQ,   Geq);
      (T.OP_LT,    Lt);
      (T.OP_GT,    Gt);
    ];
    (* bit shifts would go here - I'm not currently implementing
       them *)
    [
      (T.OP_PLUS,  Add);
      (T.OP_MINUS, Sub);
    ];
    [
      (T.OP_STAR,  Mult);
      (T.OP_DIV,   Div);
      (T.OP_MOD,   Mod);
    ];
    (* -- *)
    (* various unary operators go here, but we don't
       parse them using the same code so they don't go here.
       type casts - which seem tricky to parse - also go here. *)
    (* -- *)
    (* stuff that binds higher than unary operators, such as
       array indexing, pointer/struct lookups, postfix operators,
       and parentheses go here. The pointer/struct lookups would
       in fact be parsed as binary operations, but we wouldn't
       use the `parse_bin_expr_by_prec` function because there
       are non-binary operators with precedence "sandwiched"
       in between *)
  ]

let parse_bin_expr_one_level
    ~(parse_inner: T.tokens -> expr * T.tokens)
    ~(matching_ops: (T.token, binary_op, 'cmp) Map.t)
    (tokens: T.tokens) =
  (* get the first inner expression. We don't expect this to fail. *)
  let first_expr, after_first = parse_inner tokens in
  (* now recursively check whether we're parsing an operator at the
     same precedence level (one in `matching_ops`), and build a
     left-associative AST from the inner expressions if so *)
  let rec parse_all_terms
      (left_expr: expr)
      (after_left: T.tokens) =
    let next_token, _ = List.hd_exn after_left (* EOF makes head_exn safe *) in
    let operator = Map.find matching_ops next_token in
    match operator with
    | None ->
      left_expr, after_left
    | Some op ->
      let after_op = List.tl_exn after_left in
      let right_expr, after_right = parse_inner after_op in
      let new_expr = BinaryOp (op, left_expr, right_expr) in
      parse_all_terms new_expr after_right
  in
  parse_all_terms first_expr after_first

let rec parse_bin_expr_by_prec
    (prec: int)
    (after_highest_prec: (T.tokens -> expr * T.tokens))
    (tokens: T.tokens) =
  match Map.find binary_ops_by_prec prec with
  | None ->
    after_highest_prec tokens
  | Some matching_ops ->
    let next_prec = prec + 1 in
    let parse_inner = parse_bin_expr_by_prec next_prec after_highest_prec in
    parse_bin_expr_one_level
      ~parse_inner ~matching_ops tokens

let parse_expr tokens =
  (* Note that assignment is not a binary operator because it is
     right-associative, rather than left-associative, plus it only
     accepts an id on the left hand side *)
  let rec parse_assignment = function
  | (T.ID name, _) :: (T.OP_SEQ, _) :: more ->
    let value, rest = parse_assignment more in
    Assign (Id name, value), rest
  | tokens ->
    parse_binary tokens
  and parse_binary tokens =
    parse_bin_expr_by_prec 0 parse_unary tokens
  and parse_unary = function
    | (T.OP_MINUS, _) :: more ->
      let inner, rest = parse_unary more in
      UnaryOp (Neg, inner), rest
    | (T.OP_BANG, _) :: more ->
      let inner, rest = parse_unary more in
      UnaryOp (LNot, inner), rest
    | (T.OP_TILDE, _) :: more ->
      let inner, rest = parse_unary more in
      UnaryOp (BNot, inner), rest
    | tokens -> parse_atom tokens
  and parse_atom = function
    (* literals (constants) *)
    | (T.LIT_INT n, _) :: rest ->
      Lit (Int n), rest
    (* references (to variables) *)
    | (T.ID name, _) :: rest ->
      Reference (Id name), rest
    (* parentheses *)
    | (T.LPAREN, _) :: after_lparen ->
      let expr, after_exp = parse_assignment after_lparen in
      (match after_exp with
       | (T.RPAREN, _) :: after_rparen ->
         expr, after_rparen
       | bad_tokens -> failwith @@ emsg "expected ) parsing expr" bad_tokens
      )
    (* parse failure *)
    | bad_tokens -> failwith @@ emsg "failed to parse expr" bad_tokens
  in
  parse_assignment tokens



let parse_block_item tokens = match tokens with
  (* ==== definitions === *)
  (* NOTE: we are missing compound definitions (with commas) *)
  (* definition without initializer *)
  | (T.KW_INT, lineno) :: (T.ID varname, _) :: (T.SEMICOL, _) :: rest ->
    let def_var = DefVar {annot=IntAnnot; id=Id varname; init=None} in
    Definition (def_var, Line lineno), rest
  (* definition with initializer *)
  | (T.KW_INT, lineno) :: (T.ID varname, _) :: (T.OP_SEQ, _) :: after_eq ->
    let init, after_expr = parse_expr after_eq in
    (match after_expr with
     | (T.SEMICOL, _) :: rest ->
       let def_var = DefVar {annot=IntAnnot; id=Id varname; init=Some init} in
       Definition (def_var, Line lineno), rest
     | _ ->
       failwith @@ emsg "expected semicolon after expr" after_expr
    )
  (* ==== statements === *)
  (* return statement *)
  | (T.KW_RETURN, lineno) :: more_tokens ->
    let value, after_exp = parse_expr more_tokens in
    (match after_exp with
     | (T.SEMICOL, _) :: rest ->
       Statement (Return value, Line lineno), rest
     | _ -> failwith @@ emsg "expected semicolon in return stmt" after_exp
    )
  (* empty expression statement *)
  | (T.SEMICOL, lineno) :: rest ->
    Statement (Expr None, Line lineno), rest
  (* nonempty expression statement *)
  | (_, lineno) :: _ ->
    let expr, after_expr = parse_expr tokens in
    (match after_expr with
     | (T.SEMICOL, _) :: rest ->
       Statement (Expr (Some expr), Line lineno), rest
     | _ ->
       failwith @@ emsg "expected semicolon in expr stmt" after_expr)
  (* ==== parse fail: not really possible here b/c of EOF === *)
  | _ ->
    failwith @@ "Should not get here - expected an EOF token"


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
    | bad_tokens ->
      failwith @@ emsg "parse_fn_arg_list" bad_tokens
  in
  let parse_fn_body = function
    | (T.SEMICOL, _) :: rest ->
      (None, rest)
    | (T.LBRACE, _) :: more ->
      let block, rest = parse_block more in
      (Some block, rest)
    | bad_tokens ->
      failwith @@ emsg "parse_fn_body" bad_tokens
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
