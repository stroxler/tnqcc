open Base
open Ast
open Context

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


let emsg (line: line) (err_happened_in: string) (display: string) =
  let Line lineno = line in
  Printf.sprintf "Could not generate at line %d\nContext: %s\nAst: %s"
    lineno err_happened_in display


(* Linux and osx have different function label conventions
   In Linux the label is just the function name, in OSX we need
   an underscore prefix. That's what the `mangled_name` stuff is about *)
let get_fn_label fname =
  let mangled_name =
    if Util.equal_os_type Util.current_os Util.Linux
    then fname
    else "_" ^ fname
  in
  mangled_name

(* this is used to generat return statements *)
let get_fn_ret_label fname =
  ".L_" ^ fname ^ "__return"

let get_label () =
  label_counter := (!label_counter + 1);
  ".L" ^ (Int.to_string !label_counter)

and jump_to_if_eax label true_false =
  let jump_code = match true_false with
    | false -> "je"
    | true -> "jne"
  in (
    "  cmpl $0, %eax" ^ "\n" ^
    "  " ^ jump_code ^ " " ^ label
  )

let rec gen_block: context -> block -> (context * string) =
  fun parent_ctx body ->
    let initial_ctx = enter_block parent_ctx in
    let ending_ctx, code_lines =
      List.fold_map ~init: initial_ctx ~f:gen_block_item body in
    let code = String.concat code_lines in
    let final_ctx = exit_block ending_ctx in
    final_ctx, code

(** gen_block_item returns `context * code`
    because we have to be able to make vars and the ebp offset
    visible to down to lower code, and the maximum esp offset up to function
    declarations

    Also, block items (statements and definitions) get newlines, whereas
    expressiions do not.
*)
and gen_block_item (ctx: context) = function
  | Statement (s, l) -> gen_statement ctx l s
  | Definition (d, l) -> gen_definition ctx l d

(** gen_definition retursn `context * code` because we need to
    make later block_items in the same block able to access defined vars /
    ebp offset levels, and we have to pass up esp offset information to function
    declaration *)
and gen_definition (initial_ctx: context) l (d: def_var) =
  let DefVar { id; annot; init} = d in
  let new_ctx = add_var l (id, annot) initial_ctx in
  let code = match init with
    | None ->
      ""
    | Some e ->
      (gen_assignment new_ctx id l e) ^ "\n"
  in
  new_ctx, code

and gen_assignment (ctx: context) (Id varname) l e =
  let ram_loc = find_ram_loc l (Id varname) ctx in
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
    (jump_to_if_eax branch1_label false) ^ "\n" ^
    branch0_asm ^ "\n" ^
    "  jmp " ^ finished_label ^ "\n" ^
    branch1_label ^ ":" ^ "\n" ^
    branch1_asm ^ "\n" ^
    finished_label ^ ":" ^ "\n"
  )


and gen_loop
    (ctx: context)
    (l: line)
    ?(decl: def_var option = None)
    ?(e_init: expr option = None)
    ?(e_condition: expr option = None)
    ?(e_bottom: expr option = None)
    ?(is_do=false)
    (body_stmt: statement) =
  let loop_prefix = get_label () in
  let top_label = loop_prefix ^ "_top" in
  let commited_label = loop_prefix ^ "_committed" in
  let bottom_label = loop_prefix ^ "_bottom" in
  let done_label = loop_prefix ^ "_done" in
  (* handle allocating any declared variables *)
  let ctx_start, decl_cmd_maybe = (
    match decl with
    | None ->
      ctx, ""
    | Some d ->
      gen_definition (enter_block ctx) l d
  ) in
  (* handle
     - initializations, if any,
     - jumping directly to after the decision, if this is a
       do loop
     - setting the top label
     - the comparison used to terminate, if any
     - setting the committed label *)
  let header = (
    let do_preamble_maybe = ( match is_do with
        | true ->
          "  jmp " ^ commited_label ^ "\n"
        | false ->
          ""
      ) in
    let init_cmd_maybe = ( match e_init with
        | None ->
          ""
        | Some e ->
          (gen_expr ctx_start l e) ^ "\n"
      ) in
    let top_line = top_label ^ ":\n" in
    let committed_line_maybe =
      if is_do
      then commited_label ^ ":\n"
      else "" in
    let jump_to_done_maybe = ( match e_condition with 
        | Some e ->
          (
            (gen_expr ctx_start l e) ^ "\n" ^
            (jump_to_if_eax done_label false) ^ "\n"
          )
        | None ->
          ""
      ) in
    (
      decl_cmd_maybe ^
      init_cmd_maybe ^
      do_preamble_maybe ^
      top_line ^
      jump_to_done_maybe ^
      committed_line_maybe
    )
  ) in
  (* handle the body, which is the same for all loop types *)
  let ctx_loop = enter_loop loop_prefix ctx_start in
  let ctx_end, body = gen_statement ctx_loop l body_stmt in
  (* handle the footer:
     - the bottom label
     - the third expression in for loop, if any
     - jumping to the top (always)
     - the done label *)
  let footer = (
    let bottom_line = bottom_label ^ ":\n" in
    let done_line = done_label ^ ":\n" in
    let finish_cmd_maybe = ( match e_bottom with
        | None ->
          ""
        | Some e ->
          (gen_expr ctx_start l e) ^ "\n"
      ) in
    let jump_to_top = "  jmp " ^ top_label ^ "\n" in
    (
      bottom_line ^
      finish_cmd_maybe ^
      jump_to_top ^
      done_line
    )
  ) in
  (* handle deallocating any declared variables *)
  let ctx_final = (
    match decl with
    | None -> 
      exit_loop ctx_end
    | Some _ ->
      exit_block @@ exit_loop ctx_end
  ) in
  let code = (
    header ^
    body ^
    footer
  ) in
  ctx_final, code

(** gen_statement returns `context * code` because we have to be
    able to pass esp offset information up to the function declaration
    from any block statement that has variable declarations *)
and gen_statement (ctx: context) l stmt = match stmt with
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
        failwith @@ emsg
          l "return statement outside function" (show_statement stmt)
    ) in
    let code = (
      expr_cmd ^ "\n" ^
      close_stack_frame ^ "\n" ^
      ret_cmd ^ "\n"
    ) in
    ctx, code
  | Conditional (e, s0, s1) ->
    let expr_cmd = gen_expr ctx l e in
    let ctx0, branch0_cmd = gen_statement ctx l s0 in
    let ctx1, branch1_cmd = gen_statement ctx0 l s1 in
    let cmd = gen_conditional expr_cmd branch0_cmd branch1_cmd in
    ctx1, cmd
  | Block block ->
    gen_block ctx block
  (* loop structures; unfortunately it was hard to share code *)
  | WhileLoop (e, s) ->
    gen_loop
      ctx l s
      ~e_condition:(Some e)
  | DoLoop (s, e) ->
    gen_loop
      ctx l s
      ~e_condition:(Some e)
      ~is_do:true
  | ForLoop (ei, ec, eb, s) ->
    gen_loop
      ctx l s
      ~e_init:ei
      ~e_condition:ec
      ~e_bottom:eb
  | ForLoopDecl (d, ec, eb, s) ->
    gen_loop
      ctx l s
      ~decl:(Some d)
      ~e_condition:ec
      ~e_bottom:eb
  (* loop control statements *)
  | Break ->
    let loop_prefix = get_loop_prefix l ctx in
    ctx, "  jmp " ^ loop_prefix ^ "_done"
  | Continue ->
    let loop_prefix = get_loop_prefix l ctx in
    ctx, "  jmp " ^ loop_prefix ^ "_bottom"

and gen_expr (ctx: context) l e = match e with
  | Assign (the_id, val_expr) ->
    gen_assignment ctx the_id l val_expr
  | Lit (Int n) ->
    Printf.sprintf "  movl $%d, %s" n "%eax"
  | Reference the_id ->
    let ram_loc = find_ram_loc l the_id ctx in
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
  | Call (Id fname, args) ->
    gen_fn_call ctx l fname args


and gen_fn_call (ctx: context) l fname arg_exprs =
  (* gather information *)
  let (return_type, arg_types) = find_fn_info l fname ctx in
  let n_args_defn = List.length arg_types in
  let n_args_call = List.length arg_exprs in
  let all_args_int = List.fold arg_types
      ~init:true ~f:(fun ok a -> ok && (equal_annot a (Annot "int"))) in
  (* do some sanity checks on the function call *)
  (
    if not (phys_equal n_args_call n_args_defn)
    then failwith @@ emsg l ("calling " ^ fname) (
        Printf.sprintf "called with %d args, but decltn has %d args"
          n_args_call n_args_defn
      )
    else if not (equal_annot return_type (Annot "int"))
    then failwith @@ emsg l "non-int function returns not implemented" "fname";
    if not all_args_int
    then failwith @@ emsg l "non-int function args not implemented" "fname";
    (* actually generate code

       Note that the offsets we use relative to esp (which start at
       one word above esp) are two words smaller than the offsets we use, when
       handling ram lookups in Context, for offsets to ebp. The reason for this
       is that esp, after incrementing it for function call arguments, should
       point at the last word of the leftmost argument. It gets incremented
       twice afterward: once to push esi in the actual `call` instruction, and
       again to push ebp in the function prologue.

       Also note that we are not using push here, and this allows us to evaluate
       the arguments left-to-right. Instead of evaluating right-to-left and pushing,
       we just reserve space for the args ahead of time.

       This may be less efficient than pushing, and might not work well for
       varargs. Also, note that the C standard does not specify the order in which
       one must evaluate args (nor is the order of args on the stack technically
       specified, although gcc always does right-to-left)
    *)
    (
      let Line lineno = l in
      let arg_expr_cmds = List.map arg_exprs
          ~f:(fun e -> gen_expr ctx l e) in
      let total_args_space, offsets_per_arg = (
        let initial_esp_offset = 0 in
        let compute_offset prev_offset (a: annot) =
          let this_arg_space = get_obj_size lineno "(compiler bug)" a in
          let this_arg_offset = prev_offset + this_arg_space in
          (this_arg_offset, this_arg_offset - word_size)
        in
        List.fold_map arg_types ~f:compute_offset ~init:initial_esp_offset
      ) in
      let esp_prologue = Printf.sprintf "  subl $%d, %s" total_args_space "%esp" in
      let esp_epilogue = Printf.sprintf "  addl $%d, %s" total_args_space "%esp" in
      let stack_alignment_prologue, stack_alignment_epilogue =
        gen_stack_alignment total_args_space in
      let generate_and_push_exprs = List.map2_exn
          arg_expr_cmds offsets_per_arg
          ~f:(fun e_cmd offset ->
              (e_cmd ^ "\n" ^
               (Printf.sprintf "  movl %%eax, %d(%%esp)" offset))) in
      let call_fn = "  calll " ^ (get_fn_label fname) in
      (
        stack_alignment_prologue ^ "\n" ^
        esp_prologue ^ "\n" ^
        (String.concat ~sep:"\n" generate_and_push_exprs) ^ "\n" ^
        call_fn ^ "\n" ^
        esp_epilogue ^ "\n" ^
        stack_alignment_epilogue
      )
    )
  )


(* See https://norasandler.com/2018/06/27/Write-a-Compiler-9.html
   for details about what's going on here; the issue is that on osx
   (but not on linux) we have to always adjust the stack to be 16-byte
   aligned before calling standard library functions.

   Also, see a rant about it:
   https://blog.therealoracleatdelphi.com/2010/01/its-my-stack-frame-i-dont-care-about_14.html

   Our compiler isn't fancy enough to do this in a smart way
   (we'd need to add the current esp offset to our context
   and be constantly adjusting it), so we do it dynamically via
   some assembly-language arithmetic.

   Note that we don't have to do this to make our compiler
   internally consistent; you can swap out the comment at the bottom of
   this function, and you'll see that all of the integ tests
   except for the hello_world example work. But we get a segfault
   on the hello world example if we don't have stack alignment. My
   understanding is that the issue arises specifically on system calls.

   Some sources claim you can use -mstackrealign to compile non-aligned
   assembly, but I have not found that to be the case.
*)
and gen_stack_alignment total_args_space =
  if Util.equal_os_type Util.current_os Util.Linux
  then "", ""
  else
    let _prologue = (
      (* put what *will* be the value of esp after we add all args
         into eax. The `+ word_size` is needed because, at the bottom
         of the prologue, we have to save the adjustment itself which
         adds an extra word of storage *)
      "  movl %esp, %eax\n" ^
      "  subl $" ^ (Int.to_string (total_args_space + word_size)) ^ ", %eax\n" ^
      (* zero out edx, then put 16 into ecx so that we can
         divide eax by 16 *)
      "  xorl %edx, %edx\n" ^
      "  movl $16, %ecx\n" ^
      "  idivl %ecx\n" ^
      (* now edx has the remainder - this is the amount by which the
         future value of esp needs to be adjusted. We also save it
         to edx *)
      "  subl %edx, %esp\n" ^
      "  pushl %edx\n"
    ) in
    let _epilogue = (
      (* pop the previously computed stack alignment adjustment
         and undo whatever we did *)
      "  popl %edx\n" ^
      "  addl %edx, %esp"
    ) in
    (* "", "" *)
    (_prologue, _epilogue)

and gen_unary_expr (ctx: context) l o e =
  let expr_cmd = gen_expr ctx l e in
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


and gen_binary_expr (ctx: context) l o e0 e1 =
  (* Most operators are eager, and can reuse a lot of boilerplate in
     `gen_eager_binary`, but logical operators are lazy, hence special *)
  if Set.mem binary_ops_with_special_rules o
  then gen_special_binary_expr ctx l o e0 e1
  else
    let expr0_cmd = gen_expr ctx l e0 in
    let expr1_cmd = gen_expr ctx l e1 in
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

and gen_special_binary_expr (ctx: context) l o e0 e1 =
  match o with
  | LAnd ->
    gen_logical_combine_expr
      ctx l e0 e1
      ~cond_jmp_vs_zero: "je"
      ~short_circut_val: "0"
      ~default_val: "1"
  | LOr ->
    gen_logical_combine_expr
      ctx l e0 e1
      ~cond_jmp_vs_zero: "jne"
      ~short_circut_val: "1"
      ~default_val: "0"
  | _ -> 
    failwith (emsg l "gen_binary: not implemented operator" (show_binary_op o))


and gen_logical_combine_expr
    (ctx: context) l e0 e1
    ~cond_jmp_vs_zero ~short_circut_val ~default_val =
  let label_prefix = get_label () in
  let evaluate_to_sc_label = label_prefix ^ "_evaluate_to_sc" in
  let finished_label = label_prefix ^ "_finished" in
  let jump_to_finished = "  jmp " ^ finished_label in
  let expr0_cmd = gen_expr ctx l e0 in
  let expr1_cmd = gen_expr ctx l e1 in
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


let gen_fn_body initial_ctx l body_block =
  let final_ctx, code = gen_block initial_ctx body_block in
  let Context { fn_name; _ } = initial_ctx in
  let is_main = Option.exists fn_name ~f:(fun fn -> String.equal fn "main") in
  if is_main
  then match List.last body_block with
    | Some (Statement (Return _, _)) ->
      final_ctx, code
    | _ ->
      let _, ansi_required_return = gen_statement
          final_ctx l (Return (Lit (Int 0))) in
      final_ctx, code ^ ansi_required_return
  else
    final_ctx, code

let gen_fn_definition line ctx fname args body =
  let entered_ctx = enter_fn
      line fname args ctx in
  let Context final_ctx, body_asm = gen_fn_body entered_ctx line body in
  let fn_label = get_fn_label fname in
  let decl = Printf.sprintf "  .globl %s" fn_label in
  let frame_offset = (match final_ctx.esp_offset with
      | None -> failwith "had no esp offset in function (compiler bug)"
      | Some offset -> offset
    )in
  let esp_to_end_of_frame = Printf.sprintf "subl $%d, %s" frame_offset "%esp" in
  let esp_to_start_of_frame = Printf.sprintf "addl $%d, %s" frame_offset "%esp" in
  let open_stack_frame = (
    "  pushl	%ebp" ^ "\n" ^
    "  movl	%esp, %ebp" ^ "\n" ^
    "  " ^ esp_to_end_of_frame
  ) in
  let prologue = (
    decl ^ "\n" ^
    fn_label ^ ":" ^ "\n" ^
    open_stack_frame
  ) in
  let epilogue = (
    (get_fn_ret_label fname) ^ ":" ^ "\n" ^
    "  " ^ esp_to_start_of_frame ^ "\n" ^
    "  ret"
  ) in
  let code = (
    "\n\n" ^
    prologue ^ "\n" ^
    body_asm ^ "\n" ^
    epilogue ^ "\n"
  ) in
  (exit_fn (Context final_ctx)), code

(* Generate a function. This might not involve any code
   if the Ast element is just a declaration (although we'll update the function
   lookup map). gen_fn_definition does all the actual assembly generation,
   for definitions (that have a body)
*)
let gen_fn (parent_ctx: context) = function
  | DefFn {annot=annot; name=Id fname; args=args; body=maybe_body; line=line} ->
    let declared_ctx = declare_fn
        line fname annot (List.map ~f:(fun (_, a) -> a) args)
        (Option.is_some maybe_body) parent_ctx in
    (
      match maybe_body with
      | None ->
        declared_ctx, ""
      | Some body ->
        gen_fn_definition line declared_ctx fname args body
    )


let gen_prog = function
  | Prog fns ->
    let _final_ctx_, function_codes =
      List.fold_map ~init: empty_context ~f:gen_fn fns in
    let code = String.concat @@ function_codes in
    code


let gen = gen_prog

