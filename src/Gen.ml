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
    let ret_cmd = "\tret" in
    expr_cmd ^ "\n" ^ ret_cmd
  | bad -> failwith @@ emsg l "gen_statement" @@ show_statement bad

and gen_expr l e = match e with
  | Lit (Int n) ->
    Printf.sprintf "\tmovl $%d, %s" n "%eax"
  | UnaryOp (o, e0) ->
    gen_unary l o e0
  | BinaryOp (o, e0, e1) ->
    gen_binary l o e0 e1
  (* bad -> failwith @@ emsg l "gen_expr" @@ show_expr bad *)
  (* ^ depending on the stage of development, the
     compiler sometimes tells us there is no unhandled case *)

and gen_unary l o e =
  let expr_cmd = gen_expr l e in
  let op_cmd = match o with
    | Neg ->
      "\tneg %eax"
    | BNot ->
      "\tnot %eax"
    | LNot ->
      "\tcmp $0, %eax\n" ^  (* set flags based on whether eax is 0 *)
      "\tmovl $0, %eax\n" ^    (* zero out eax *)
      "\tsete %al"          (* set eax based on flags: 1 if the prev comparison was
                               e = equal .. also called setz for "zero" *)
  in
  expr_cmd ^ "\n" ^ op_cmd

and gen_binary l o e0 e1 =
  let expr0_cmd = gen_expr l e0 in
  let expr1_cmd = gen_expr l e1 in
  let op_cmd = gen_binary_op l o in
  (expr0_cmd ^ "\n" ^
   "\tpush %eax" ^ "\n" ^  (* pushes to ESP address, increments ESP *)
   expr1_cmd ^ "\n" ^
   op_cmd)                 (* note that op_cmd *must* pop somewhere *)


and gen_binary_op l o =
  let pop_reg_then reg code = "\tpop %" ^ reg ^ "\n" ^ code in
  (* idvl reg performs (edx:eax) / reg, storing the quotient in eax and
     remainder in edx. This sets the registers up. *)
  let perform_idiv_then code = (
    "\tmovl %eax, %ecx" ^ "\n" ^
    "\tmovl $0, %edx" ^ "\n" ^
    "\tpop %eax" ^ "\n" ^
    "\tidiv %ecx" ^
    match code with
    | "" -> ""
    | _ -> "\n" ^ code
  ) in
  match o with
  | Add ->
    pop_reg_then "ecx" "\taddl %ecx, %eax"
  | Sub ->
    ("\tmovl %eax, %ecx" ^ "\n" ^  (* subl's operand order is funny *)
     pop_reg_then "eax" "\tsubl %ecx, %eax")
  | Mult ->
    pop_reg_then "ecx" "\timul %ecx, %eax"
  | Div ->
    perform_idiv_then ""
  | Mod ->
    perform_idiv_then "\tmovl %edx, %eax"
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
    let decl = Printf.sprintf "\t.globl %s" mangledName in
    let label = Printf.sprintf "%s:" mangledName in
    decl ^ "\n" ^ label ^ "\n" ^ (gen_block body) ^ "\n\n"
  | DefFn {annot=_; name=_; body=None; line=_; } ->
    ""

let gen = function
  | Prog fns -> String.concat @@ List.map ~f:gen_fn fns

