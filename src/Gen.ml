open Base
open Ast

module Context = Context  (* Make Context available for unit tests *)

(**
   At a high-level, you can think of the expression handling as a stack
   machine with the following rules:
   - all expressions evaluate in eax
   - unary operations evaluate the inner expression, then operate on eax
   - binary operations
     - evaluate the first inner expression, then push eax to the stack
     - evaluate the second inner expression, which is now in eax
     - pop from the stack into some suitable register
     - perform whatever operations are needed
     - ensure the result is in eax
     - Note: in general we *must* use the stack in a non-optimizing compiler
       because expressions are recursive, so if we tried to store the first
       operand in a register we couldn't guarantee that some operation involved
       in evaluating the second operand would not erase it. An optimizing
       compiler could use registers some of the time.
*)

let label_counter = ref 0

let binary_ops_with_special_rules = Set.of_list (module BinaryOp) [
    LAnd; LOr;
  ]



let get_fn_ret_label fname =
  ".L_" ^ fname ^ "__return"


let get_label () =
  label_counter := (!label_counter + 1);
  ".L" ^ (Int.to_string !label_counter)

let emsg (line: line) (context: string) (display: string) =
  let Line lineno = line in
  Printf.sprintf "Could not generate at line %d\nContext: %s\nAst: %s"
    lineno context display

let rec gen_block: Context.context -> block -> (Context.context * string) =
  fun parent_ctx body ->
    let initial_ctx = Context.add_slevel parent_ctx in
    let ending_ctx, code_lines =
      List.fold_map ~init: initial_ctx ~f:gen_block_item body in
    let code = String.concat code_lines in
    let final_ctx = Context.remove_slevel ending_ctx in
    final_ctx, code

(** gen_block_item returns `context * code`
    because we have to be able to make vars and the ebp offset
    visible to down to lower code, and the maximum esp offset up to function
    declarations

    Also, block items (statements and definitions) get newlines, whereas
    expressiions do not.
*)
and gen_block_item (ctx: Context.context) = function
  | Statement (s, l) -> gen_statement ctx l s
  | Definition (d, l) -> gen_definition ctx l d

(** gen_definition retursn `context * code` because we need to
    make later block_items in the same block able to access defined vars /
    ebp offset levels, and we have to pass up esp offset information to function
    declaration *)
and gen_definition (initial_ctx: Context.context) l (d: def_var) =
  let DefVar { id; annot; init} = d in
  let new_ctx = Context.add_var l (id, annot) initial_ctx in
  let code = match init with
    | None ->
      ""
    | Some e ->
      (gen_assignment new_ctx id l e) ^ "\n"
  in
  new_ctx, code

and gen_assignment (ctx: Context.context) (Id varname) l e =
  let ram_loc = Context.find_ram_loc l (Id varname) ctx in
  let expr1_cmd = gen_expr ctx l e in
  let mov_cmd = "  movl %eax, " ^ ram_loc in
  (
    expr1_cmd ^ "\n" ^
    mov_cmd
  )

(** Generic conditional assembly language construct, which is
    usable regardless of whether the branches are expressions (as
    in the ternary operator) or statements (as in conditional
    statements *)
and gen_conditional
    expr_asm branch0_asm branch1_asm =
  let label_prefix = get_label () in
  let branch1_label = label_prefix ^ "_branch1" in
  let finished_label = label_prefix ^ "_finished" in
  (
    expr_asm ^ "\n" ^           (* evaluate the if expr *)
    "  cmpl $0, %eax" ^ "\n" ^  (* compare to false *)
    "  je " ^ branch1_label ^ "\n" ^
    branch0_asm ^ "\n" ^
    "  jmp " ^ finished_label ^ "\n" ^
    "  " ^ branch1_label ^ ":" ^ "\n" ^
    branch1_asm ^ "\n" ^
    "  " ^ finished_label ^ ":"
  )


(** gen_statement returns `context * code` because we have to be
    able to pass esp offset information up to the function declaration
    from any block statement that has variable declarations *)
and gen_statement (ctx: Context.context) l s = match s with
  | Expr None ->
    ctx, ""
  | Expr (Some e) ->
    ctx, (gen_expr ctx l e) ^ "\n"
  | Return e ->
    let expr_cmd = gen_expr ctx l e in
    let close_stack_frame = "  popl	%ebp" in (* see comments on gen_fn for details *)
    let ret_cmd = (
      let Context { fn_name; _ } = ctx in
      match fn_name with
      | Some fname ->
        "  jmp " ^ (get_fn_ret_label fname)
      | None ->
        failwith @@ emsg l "return statement outside function" @@ show_statement s
    ) in
    let code = expr_cmd ^ "\n" ^ close_stack_frame ^ "\n" ^ ret_cmd ^ "\n" in
    ctx, code
  | Conditional (e, s0, s1) ->
    let expr_cmd = gen_expr ctx l e in
    let ctx0, branch0_cmd = gen_statement ctx l s0 in
    let ctx1, branch1_cmd = gen_statement ctx0 l s1 in
    let cmd = gen_conditional expr_cmd branch0_cmd branch1_cmd in
    ctx1, cmd
  | Block block ->
    gen_block ctx block
(* | bad -> failwith @@ emsg l "gen_statement" @@ show_statement bad *)

and gen_expr (ctx: Context.context) l e = match e with
  | Assign (the_id, val_expr) ->
    gen_assignment ctx the_id l val_expr
  | Lit (Int n) ->
    Printf.sprintf "  movl $%d, %s" n "%eax"
  | Reference the_id ->
    let ram_loc = Context.find_ram_loc l the_id ctx in
    Printf.sprintf "  movl %s, %s" ram_loc "%eax"
  | UnaryOp (o, e0) ->
    gen_unary_expr ctx l o e0
  | BinaryOp (o, e0, e1) ->
    gen_binary_expr ctx l o e0 e1
  | TrinaryOp (e0, e1, e2) ->
    gen_conditional
      (gen_expr ctx l e0)
      (gen_expr ctx l e1)
      (gen_expr ctx l e2)
(* | bad -> failwith @@ emsg l "gen_expr" @@ show_expr bad *)
(* ^ depending on the stage of development, the
   compiler sometimes tells us there is no unhandled case *)

and gen_unary_expr (context: Context.context) l o e =
  let expr_cmd = gen_expr (context: Context.context) l e in
  let op_cmd = match o with
    | Neg ->
      "  neg %eax"
    | BNot ->
      "  not %eax"
    | LNot ->
      "  cmp $0, %eax\n" ^  (* set flags based on whether eax is 0 *)
      "  movl $0, %eax\n" ^    (* zero out eax *)
      "  sete %al"          (* set eax based on flags: 1 if the prev comparison was
                               e = equal .. also called setz for "zero" *)
  in
  expr_cmd ^ "\n" ^ op_cmd


and gen_binary_expr (context: Context.context) l o e0 e1 =
  (* Most operators are eager, and can reuse a lot of boilerplate in
     `gen_eager_binary`, but logical operators are lazy, hence special *)
  if Set.mem binary_ops_with_special_rules o
  then gen_special_binary_expr context l o e0 e1
  else
    let expr0_cmd = gen_expr context l e0 in
    let expr1_cmd = gen_expr context l e1 in
    let op_cmd = gen_binary_op l o in
    (expr0_cmd ^ "\n" ^
     "  push %eax" ^ "\n" ^  (* pushes to ESP address, increments ESP *)
     expr1_cmd ^ "\n" ^
     op_cmd)                 (* note that op_cmd *must* pop exactly once *)


and gen_binary_op l o =  (* only use for eager ops (all but &&/||) *)
  let pop_reg_then reg code = "  pop %" ^ reg ^ "\n" ^ code in
  let pop_ecx_then = pop_reg_then "ecx" in
  (* idvl reg performs (edx:eax) / reg, storing the quotient in eax and
     remainder in edx. This sets the registers up. *)
  let perform_idiv_then code = (
    "  movl %eax, %ecx" ^ "\n" ^
    "  movl $0, %edx" ^ "\n" ^
    "  pop %eax" ^ "\n" ^
    "  idiv %ecx" ^
    match code with
    | "" -> ""
    | _ -> "\n" ^ code
  ) in
  let perform_comparison_then code = (
    pop_ecx_then (
      "  cmpl %eax, %ecx" ^ "\n" ^  (* note: the order is unintuitive *)
      "  movl $0, %eax" ^ "\n" ^
      code
    )
  ) in
  match o with
  | Add ->
    pop_ecx_then "  addl %ecx, %eax"
  | Sub -> (
      "  movl %eax, %ecx" ^ "\n" ^  (* subl's operand order is funny *)
      pop_reg_then "eax" "  subl %ecx, %eax"
    )
  | Mult ->
    pop_ecx_then "  imul %ecx, %eax"
  | Div ->
    perform_idiv_then ""
  | Mod ->
    perform_idiv_then "  movl %edx, %eax"
  | Eq ->
    perform_comparison_then "  sete %al"
  | Neq ->
    perform_comparison_then "  setne %al"
  | Geq ->
    perform_comparison_then "  setge %al"
  | Leq ->
    perform_comparison_then "  setle %al"
  | Gt ->
    perform_comparison_then "  setg %al"
  | Lt ->
    perform_comparison_then "  setl %al"
  | BAnd ->
    pop_ecx_then "  and %ecx, %eax"
  | BOr ->
    pop_ecx_then "  or %ecx, %eax"
  | XOr ->
    pop_ecx_then "  xor %ecx, %eax"
  | _ ->
    failwith (emsg l "gen_binary_op: not implemented operator" (show_binary_op o))

and gen_special_binary_expr (context: Context.context) l o e0 e1 =
  match o with
  | LAnd ->
    gen_logical_combine_expr
      context l e0 e1
      ~cond_jmp_vs_zero: "je"
      ~short_circut_val: "0"
      ~default_val: "1"
  | LOr ->
    gen_logical_combine_expr
      context l e0 e1
      ~cond_jmp_vs_zero: "jne"
      ~short_circut_val: "1"
      ~default_val: "0"
  | _ -> 
    failwith (emsg l "gen_binary: not implemented operator" (show_binary_op o))


and gen_logical_combine_expr
    (context: Context.context) l e0 e1
    ~cond_jmp_vs_zero ~short_circut_val ~default_val =
  let label_prefix = get_label () in
  let evaluate_to_sc_label = label_prefix ^ "_evaluate_to_sc" in
  let finished_label = label_prefix ^ "_finished" in
  let jump_to_finished = "  jmp " ^ finished_label in
  let expr0_cmd = gen_expr (context: Context.context) l e0 in
  let expr1_cmd = gen_expr (context: Context.context) l e1 in
  let skip_if_sc = (
    "  cmpl $0, %eax" ^ "\n" ^
    cond_jmp_vs_zero ^ " " ^ evaluate_to_sc_label
  ) in
  let set_result_to_default =
    "  movl $" ^ default_val ^ ", %eax" in
  let set_result_to_sc =
    "  movl $" ^ short_circut_val ^ ", %eax" in
  let evaluate_to_sc_label_line = evaluate_to_sc_label ^ ":" in
  let finished_label_line = finished_label ^ ":" in
  (
    expr0_cmd ^ "\n" ^
    skip_if_sc ^ "\n" ^
    expr1_cmd ^ "\n" ^
    skip_if_sc ^ "\n" ^
    set_result_to_default ^ "\n" ^
    jump_to_finished ^ "\n" ^
    evaluate_to_sc_label_line ^ "\n" ^
    set_result_to_sc ^ "\n" ^
    finished_label_line
  )


let gen_fn_body initial_context l body_block =
  let final_context, code = gen_block initial_context body_block in
  let Context { fn_name; _ } = initial_context in
  let is_main = Option.exists fn_name ~f:(fun fn -> String.equal fn "main") in
  if is_main
  then match List.last body_block with
    | Some (Statement (Return _, _)) ->
      final_context, code
    | _ ->
      let _, ansi_required_return = gen_statement
          final_context l (Return (Lit (Int 0))) in
      final_context, code ^ ansi_required_return
  else
    final_context, code

(* Linux and osx have different function label conventions
   In Linux the label is just the function name, in OSX we need
   an underscore prefix. That's what the `mangled_name` stuff is about
*)
(* stack frame rules are:
   - esp always points to the top of the stack (the last non-free spot; the
     first free spot is `-4(%esp)`). `push` and `pop` auto-adjust it
   - ebp should point at the top of the stack frame for the current function
   - there are two values above %ebp that are special:
     - the last one is the previous value of ebp (top of the prior function's
       stack frame), which we manually save in `open_stack_frame`, and restore
       in the return statement's `close_stack_frame`
     - The one above it is the return instruction address, which `call`
       automatically saves for us (in some assembly languages there's no `call`;
       it's equiv to a push and a jump and `ret` is equivalent to a pop and a
       jump).
   - note that this means function arguments are two addresses above ebp. In
     32-bit mode this means the first function arg is at 8(%ebp).
*)
let gen_fn (parent_ctx: Context.context) = function
  | DefFn {annot=_; name=Id fname; body=Some body; line=line} ->
    let child_ctx = Context.new_function line fname [] parent_ctx in
    let Context.Context final_ctx, body_asm = gen_fn_body child_ctx line body in
    let mangled_name =
      if Util.equal_os_type Util.current_os Util.Linux
      then fname
      else "_" ^ fname
    in
    let decl = Printf.sprintf "  .globl %s" mangled_name in
    let label = Printf.sprintf "%s:" mangled_name in
    let frame_offset = final_ctx.esp_offset in
    let esp_to_end_of_frame = Printf.sprintf "subl $%d, %s" frame_offset "%esp" in
    let esp_to_start_of_frame = Printf.sprintf "addl $%d, %s" frame_offset "%esp" in
    let open_stack_frame = (
      "  pushl	%ebp" ^ "\n" ^
      "  movl	%esp, %ebp" ^ "\n" ^
      "  " ^ esp_to_end_of_frame
    ) in
    let close_stack_frame = (
      (get_fn_ret_label fname) ^ ":" ^ "\n" ^
      "  " ^ esp_to_start_of_frame ^ "\n" ^
      "  ret"
    ) in
    let code = (
      decl ^ "\n" ^ label ^ "\n" ^
      open_stack_frame ^ "\n" ^
      body_asm ^ "\n\n" ^
      close_stack_frame ^ "\n" ^
      "  ret"
    ) in
    child_ctx, code
  | DefFn {annot=_; name=_; body=None; line=_; } ->
    parent_ctx, ""


let gen_prog = function
  | Prog fns ->
    let _final_context, function_codes =
      List.fold_map ~init: Context.empty_context ~f:gen_fn fns in
    let code = String.concat @@ function_codes in
    code


let gen = gen_prog

