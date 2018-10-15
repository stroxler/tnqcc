open Core
open Ast

(* word size is 32 bits = 4 bytes *)
let word_size = 4
(* the first offset when looking down the stack (up in address space)
   from ebp to find variables is (word size + data object size) because ebp
   points to the previous value of ebp from the higher stack
   frame, and one word down the stack is the return instruction
   address.

   So, for example, in 32 bit x86, if the first argument is an int then
   it will live at `8(%ebp)`; if it's a double it will live at `12(%ebp)`

   Note that for args we have to track the previous offset to calculate
   the next arg, since we are going up in address space and therefore
   the current offset depends on the current data object size
*)
let initial_args_prev_ebp_offset = word_size
(* the offset when looking up the stack (down in address space)
   from ebp to find local variables. This starts as word size

   Note that for locals we have to track the current offset, because we
   are going down in address space and therefore the current offset
   does not depend on data object size; rather the next offset will.

   So, e.g., if the first local is an int or a double, in either case it will
   live at `-4(%ebp)`, but the *second* local, if any, will live at `-8(%ebp)`
   or `-12(%ebp)`.
*)
let initial_locals_current_ebp_offset = word_size


(* A scope frame is a map of variables in the current scope
   When we add a scope (e.g. in a compound statement) we add a new
   scope frame, which is like a "layer" on top of the other scope frames
   within our stack frame.

   Adding a scope frame is different than adding a stack frame, because only
   variables in the current stack frame are within scope, whereas variables from
   higher scope frames are in scope unless they have been aliased.

   Because I'm not using a proper state design pattern in the Gen code
   (I'd like to change this at some point), the scopes and a tracker in
   the stack frames are mutable. This allows us to do two things:
     - allow variable decleration block_items to modify the top scope
       without having to pass context back up, because the top scope has
       a mutable ref to the top scope var map and the current ebp offset
     - track the maximum ebp offset at the stack frame level, which in turn
       means that after generating the body of a function, we can judge where
       esp (which, at least in gcc output, points at the top of the stack after
       allocating enough room for all local variables in all scopes) ought
       to point ahead of time

   Note that stack frames themselves are not mutable, only the ebp tracker
   inside them. So we get a new stack frame every time we enter an inner
   scope, and throw it away when we return to the outer scope. But the ebp
   tracker can see what the highest ebp offset used in any scope was.

   A note on the need for an ebp offset tracker:
       - In gcc output, the *operand* stack (the part of the stack used
         for storing expression evaluation, including binary operator
         operands and arguments to function calls) is on top of (lower
         in address space than) all local variables
       - This means that local variables are never pushed. Both args and
         local vars are handled in terms of %ebp. Whereas anything pushed
         to the operand stack during expression evaluation is handled via
         `push` and `pop`, which operate on `esp` (although we rarely have
         to reference `esp` directly)
       .... it would be possible, for C, to use a different pattern ...
       - We could try to always keep `esp` pointed as far down in the stack
         as possible, and just increment it every time we add a local var.
       - Since variable declarations are not permitted within operators or
         function calls in C, this would work because the operand stack is
         always empty when we hit a variable declaration
       - But this would be less efficient, because we'd have to constantly
         futz with esp when we add local variables, instead of just setting
         it once, as part of the function header
       - Also, this would make stack management much harder if we added
         semantics like rust where you can define locals inside of an
         expression, since in that case the operand stack wouldn't necessarily
         be empty at the point where you hit a local variable declaration
 *)

let emsg (lineno: int) (msg: string) =
  "Line " ^ (Int.to_string lineno) ^ ":  " ^ msg


(* note: the first two args are only used in a failure message,
   and the failure message is unreachable except when we are
   deving new extensions to the compiler and add new types
   (e.g. when writing the parser for a new type, we won't yet
   have code generation set up)

   By prefixing the varnames with an underscore, we disable
   ocaml from throwing errors about unused variables
*)
let get_obj_size _lineno _objname = function
  | IntAnnot ->
        4
  (* | _ ->
       _lineno
       ("Could not process type annotation of argument/variable " ^
        _objname ^ " with type " ^ (show_annot annot))
   *)


type var_info = { ram_loc: string; annot: annot; }

let show_var_info vi =
  "{ ram_loc = \"" ^ vi.ram_loc ^ "\"; annot = " ^ (show_annot vi.annot) ^ "; }"

type scope_level = ScopeLevel of {
    is_arg_level: bool;
    var_infos: (string, var_info, String.comparator_witness) Map.t;
    ebp_offset: int;
  }


let show_scope_level (ScopeLevel sl) =
  let is_arg_level_str = Bool.to_string sl.is_arg_level in
  let var_infos_str = (
    "Map.of_alist (module String) " ^
    (Util.show_list
       (fun (v, vi) ->
          Printf.sprintf "(\"%s\", %s)" v (show_var_info vi))
       (Map.to_alist sl.var_infos))) in
  let ebp_offset_str = Int.to_string sl.ebp_offset in
  ("ScopeLevel { is_arg_level = " ^ is_arg_level_str ^ ";\n" ^
   "             var_infos = " ^ var_infos_str ^ ";\n" ^
   "             ebp_offset = " ^ ebp_offset_str ^ "; }")

(* for testing only *)
let _mk_scope_level is_arg_level vi_alist ebp_offset = ScopeLevel {
    is_arg_level = is_arg_level;
    var_infos = Map.of_alist_exn (module String) vi_alist;
    ebp_offset = ebp_offset;
  }

let empty_slevel ?(is_arg_level=false) (ebp_offset : int) =
  ScopeLevel {
    is_arg_level = is_arg_level;
    var_infos = Map.empty (module String);
    ebp_offset = ebp_offset;
  }


let add_obj_to_slevel
    lineno
    (obj: id * annot)
    ?(is_arg_level=false)
    (ScopeLevel slevel) =
  (* Make sure the caller knows what they are doing *)
  if not @@ phys_equal slevel.is_arg_level is_arg_level
  then failwith "Tried to add local to arguments scope (this is a bug)";
  (* parse the obj *)
  let (Id name, annot) = obj in
  let size = get_obj_size lineno name annot in
  (* See notes above about args vs local vars for why this code works as it
     does: with args we go up in address space and address depends on type size,
     whereas with locals we go down and the address is already known *)
  let cur_ebp_offset = slevel.ebp_offset in
  let new_ebp_offset = cur_ebp_offset + size in
  (* update the ram refs map. Fail (user error) if name already declared *)
  let ram_loc =
    if slevel.is_arg_level
    then Printf.sprintf  "%d(%%ebp)" @@ new_ebp_offset
    else Printf.sprintf "-%d(%%ebp)" @@ cur_ebp_offset
  in
  let var_info = { ram_loc = ram_loc; annot = annot } in
  let added_var = Map.add slevel.var_infos ~key:name ~data:var_info in
  let new_var_infos = (match added_var with
      | `Ok updated_var_infos ->
        updated_var_infos
      | `Duplicate ->
        failwith @@ emsg
          lineno
          "Duplicate variable or argument of name " ^ name
    ) in
  (* return the slevel offset, so that the stack-level
     code can track the max offset *)
  ScopeLevel {
    slevel with
    var_infos = new_var_infos;
    ebp_offset = new_ebp_offset;
  }


let slevel_from_args lineno (args: (id * annot) list) =
  let initial_slevel = empty_slevel
      initial_args_prev_ebp_offset ~is_arg_level:true in
  let rec go slevel = function
    | [] -> slevel
    | argvar :: more_args ->
      let new_slevel = add_obj_to_slevel
          lineno argvar slevel
          ~is_arg_level:true
      in go new_slevel more_args
  in
  go initial_slevel args;


type context = Context of {
    stack_frame: scope_level list;
    esp_offset: int;
    fn_name: string option;
  }


let show_context (Context ctx) =
  let stack_frame_str = Util.show_list show_scope_level ctx.stack_frame in
  let offset_str = Int.to_string ctx.esp_offset in
  let fn_name_str = match ctx.fn_name with
    | None -> "None"
    | Some n -> "Some \"" ^ n ^ "\""
  in
  ("Context { stack_frame = " ^  stack_frame_str ^ ";\n" ^
   "          esp_offset = " ^ offset_str ^ ";\n" ^
   "          fn_name = " ^ fn_name_str ^ "; }")


let empty_context = Context {
    stack_frame = [];
    esp_offset = -999;
    fn_name = None;
  }


let new_function
    (Line lineno)
    (fn_name: string)
    (args: (Ast.id * Ast.annot) list)
    (Context _parent_ctx) =
  let args_slevel = slevel_from_args lineno args in
  Context {
    (* parent_ctx with *)
    stack_frame = [args_slevel];
    esp_offset = 0;
    fn_name = Some fn_name;
  }

let add_slevel (Context ctx) =
  let ScopeLevel previous_inner_slevel = List.hd_exn ctx.stack_frame in
  let ebp_offset = previous_inner_slevel.ebp_offset in
  Context {
    ctx with
    stack_frame = (empty_slevel ebp_offset ) :: ctx.stack_frame
  }

(* Note that we do *not* reset the esp_offset *)
let remove_slevel (Context ctx) =
  let outer_slevels = List.tl_exn ctx.stack_frame in
  Context {
    ctx with
    stack_frame = outer_slevels
  }

(* At the beginning of a function, the esp pointer is one
   word lower in address space than ebp. So the total esp offset
   relative to what it would otherwise be is one word size *less*
   than the maximum ebp offset to reach the next free space, where
   the maximum is over all variable declarations in all scope
   levels of a function *)
let compute_new_esp_offset old_esp_offset current_ebp_offset =
  let minimal_esp_offset = current_ebp_offset - word_size in
  Int.max minimal_esp_offset old_esp_offset

let add_var (Line lineno) (var: id * annot) (Context ctx) =
  let old_inner_slevel, outer_slevels = (
    match ctx.stack_frame with
    | i :: o ->
      i, o
    | [] ->
      failwith "Should not have empty stack frame (this is a bug)"
  ) in
  let ScopeLevel inner_slevel = add_obj_to_slevel lineno var old_inner_slevel in
  let stack_frame = (ScopeLevel inner_slevel) :: outer_slevels in
  let esp_offset = compute_new_esp_offset ctx.esp_offset inner_slevel.ebp_offset in
  Context {
    ctx with
    stack_frame = stack_frame;
    esp_offset = esp_offset;
  }


let find_info (Line lineno) (Id objname) (Context ctx) =
  let rec go = function
    | (ScopeLevel scope) :: outer_slevels ->
      let var_infos = scope.var_infos in
      (match Map.find var_infos objname with
       | Some ram_reference ->
         ram_reference
       | None ->
         go outer_slevels
      )
    | [] ->
      failwith @@ emsg
        lineno
        "Could not find object " ^ objname ^ " in current scope"
  in go ctx.stack_frame


let find_ram_loc line the_id context =
  let info = find_info line the_id context in
  info.ram_loc


let find_type_annot line the_id context =
  let info = find_info line the_id context in
  info.annot

let is_one_level (Context ctx) =
  List.length ctx.stack_frame = 2