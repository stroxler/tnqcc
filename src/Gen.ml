open Base
open Ast


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

let lazy_binary_ops = Set.of_list (module BinaryOp) [
    LAnd; LOr;
  ]



let get_label () =
  label_counter := (!label_counter + 1);
  ".L" ^ (Int.to_string !label_counter)

let emsg (line: line) (context: string) (display: string) =
  let Line lineno = line in
  Printf.sprintf "Could not generate at line %d\nContext: %s\nAst: %s"
    lineno context display

let rec gen_block body =
  String.concat @@ List.map ~f:gen_block_item body

and gen_block_item = function
  | Statement (s, l) -> gen_statement l s
  | Definition (d, l) -> failwith @@ emsg l "gen_block_item" @@ show_def_var d

and gen_statement l s = match s with
  | Return e ->
    let expr_cmd = gen_expr l e in
    let ret_cmd = "  ret" in
    expr_cmd ^ "\n" ^ ret_cmd
  | bad -> failwith @@ emsg l "gen_statement" @@ show_statement bad

and gen_expr l e = match e with
  | Lit (Int n) ->
    Printf.sprintf "  movl $%d, %s" n "%eax"
  | UnaryOp (o, e0) ->
    gen_unary_expr l o e0
  | BinaryOp (o, e0, e1) ->
    gen_binary_expr l o e0 e1
  (* bad -> failwith @@ emsg l "gen_expr" @@ show_expr bad *)
  (* ^ depending on the stage of development, the
     compiler sometimes tells us there is no unhandled case *)

and gen_unary_expr l o e =
  let expr_cmd = gen_expr l e in
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


and gen_binary_expr l o e0 e1 =
  (* Most operators are eager, and can reuse a lot of boilerplate in
     `gen_eager_binary`, but logical operators are lazy, hence special *)
  if Set.mem lazy_binary_ops o
  then gen_lazy_binary_expr l o e0 e1
  else
    let expr0_cmd = gen_expr l e0 in
    let expr1_cmd = gen_expr l e1 in
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
      "  cmpl %eax, %ecx" ^ "\n" ^  (* not operand order is reversed in .S format *)
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

and gen_lazy_binary_expr l o e0 e1 =
    let label_prefix = get_label () in
    let logical_combine ~cond_jmp_vs_zero ~short_circut_val ~default_val =
      let evaluate_to_sc_label = label_prefix ^ "_evaluate_to_sc" in
      let finished_label = label_prefix ^ "_finished" in
      let jump_to_finished = "  jmp " ^ finished_label in
      let expr0_cmd = gen_expr l e0 in
      let expr1_cmd = gen_expr l e1 in
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
    in
    match o with
    | LAnd ->
      logical_combine
        ~cond_jmp_vs_zero: "je"
        ~short_circut_val: "0"
        ~default_val: "1"
    | LOr ->
      logical_combine
        ~cond_jmp_vs_zero: "jne"
        ~short_circut_val: "1"
        ~default_val: "0"
    | _ ->
      failwith (emsg l "gen_binary: not implemented operator" (show_binary_op o))


(* Linux and osx have different function label conventions
   In Linux the label is just the function name, in OSX we need
   an underscore prefix. *)
let gen_fn = function
  | DefFn {annot=_; name=Id name; body=Some body; line=_} ->
    let mangledName =
      if Util.equal_os_type Util.current_os Util.Linux
      then name
      else "_" ^ name
    in
    let decl = Printf.sprintf "  .globl %s" mangledName in
    let label = Printf.sprintf "%s:" mangledName in
    decl ^ "\n" ^ label ^ "\n" ^ (gen_block body) ^ "\n\n"
  | DefFn {annot=_; name=_; body=None; line=_; } ->
    ""

let gen = function
  | Prog fns -> String.concat @@ List.map ~f:gen_fn fns

