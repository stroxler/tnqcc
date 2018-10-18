open Core
open Ast


let emsg (lineno: int) (msg: string) =
  "Line " ^ (Int.to_string lineno) ^ ":  " ^ msg


let get_obj_size lineno objname = function
  | Annot "int" ->
    4
  | bad_annot ->
    failwith @@ emsg
      lineno
      ("Could not process type annotation of argument/variable " ^
       objname ^ " with type " ^ (show_annot bad_annot))

(* word size is 32 bits = 4 bytes *)
let word_size = 4

(* After the function prologue, the stack looks like this:
   - both ebp and esp point at the previous value of ebp
   - the return address (previous esi value) is one above ebp (because `call`
     does an implicit `push esi`)
   - function args are in left-to-right order as we move up in address space,
     starting at two words higher than `ebp`
   - during our function execution, ebp stays fixed (except in the epilogue); it
     changes during calls, but always gets reset
   - local vars (including inner-block-scoped vars) will take some possibly-zero
     amount of space, starting one word lower than ebp (which, again, points at
     the old ebp) in address space
   - in part of the prologue, we'll want to move esp to the last word of
     local vars, so that any push operations start one word beyond all
     local vars (push gets used when computing expressions, and either push
     or equivalent esp nonsense gets used in function calls depending on the
     implementation).
   - in Context, we're wanting to track ebp offsets so that we know where
     various local variables live. To do this, we always need to track the
     offset to the *next* local var. Local vars live at ebp *minus* some
     offset; we start the next offset out at one word so that we don't
     clobber the previous ebp value, which lives at `(%ebp)`
   - when we're creating the lookup map for arguments - which are also
     local variables - we do a similar thing, but we always track the
     *previous* offset instead of the *next* offset, and we go *higher*
     in address space as we run through the args left-to-right. The
     last word in function args needs to be two words above ebp,
     because one word above ebp is the return instruction address.

   As a result of these considerations, both of our initial offsets
   start at one word.
*)
let initial_args_prev_ebp_offset = word_size
let initial_locals_current_ebp_offset = word_size

(* At the beginning of a function, the esp pointer has the
   same value as the ebp pointer; it points at the previous
   ebp value. This is why ebp offsets for local variables start
   at `4(%ebp)`.

   But the esp *offset* is the amount we need to *add* to esp to safely push
   without clobbering local storage. Since a push incrementes esp before
   pushing, esp should point at (*at*, not *past*) the last byte of the last bit
   of local storage needed anywhere in the function.

   Now, when we track ebp offsets we are always tracking the
   next offset we would use, rather than the last offset we did
   use. As a result, the updated esp offset is the max of whatever it
   is so far (ebp offsets don't grow linearly because of inner
   block scopes) and one word *less* than whetever the next available
   ebp offset would be.
*)
let compute_new_esp_offset old_esp_offset next_available_ebp_offset =
  let minimal_esp_offset = next_available_ebp_offset - word_size in
  Int.max minimal_esp_offset old_esp_offset



(* A stack frame is the fundamental place for local variable storage
   in non-block-soped languages like python, but in languages like
   C (and most other compiled languages) there are inner scopes; in
   the case of C any compound statement (curly brace delimited) creates
   a new block scope, and also there's an implicit scope created by
   for loops that declare a new variable (it can shadow an outer variable).

   So, our variable lookup for a stack frame has to be implemented as a stack
   (list) of block scopes, where we search the stack top (left) to bottom
   (right). Each block is just a regular map from varname to a memory
   reference. For the sake of semantic analysis, we also store the type
   annotation (and we'd add other info, such as const modifiers, if we
   were making a full compiler).

   We also need to track the current ebp offset for each scope, which starts out
   at the offset of the outer scope at the time this scope was made and grows
   down as variables are declared (tracking ebp offsets at the block scope level
   lets us reclaim stack space once a block goes out-of-scope).

   That's all there has to be to a block scope, because logic can live
   \at other layers of the software. But I decided to put the variable-adding
   logic into the block scope. As a result, our block scope also has to know
   whether it's the outermost scope of a function because the logic for memory
   offsets is different (we go up in address space, not down). See the notes
   above for more details on this.

   Also note that we have a next_bscope_ebp_offset function method that
   has to know whether the current scope is an argument scope, because
   in that case we ignore the ebp offset (which was going up in address
   space and is irrelevant to local variables). Again, I mildly regret
   this implementation; keeping logic in a higher level might have
   simplified things.
*)


type var_info = { ram_loc: string; annot: annot; }

let show_var_info vi =
  "{ ram_loc = \"" ^ vi.ram_loc ^ "\"; annot = " ^ (show_annot vi.annot) ^ "; }"

type block_scope = BlockScope of {
    is_arg_scope: bool;
    var_info_map: (string, var_info, String.comparator_witness) Map.t;
    ebp_offset: int;
  }


let show_block_scope (BlockScope sl) =
  let is_arg_scope_str = Bool.to_string sl.is_arg_scope in
  let var_info_str = (
    "Map.of_alist (module String) " ^
    (Util.show_list
       (fun (v, vi) ->
          Printf.sprintf "(\"%s\", %s)" v (show_var_info vi))
       (Map.to_alist sl.var_info_map))) in
  let ebp_offset_str = Int.to_string sl.ebp_offset in
  ("BlockScope { is_arg_scope = " ^ is_arg_scope_str ^ ";\n" ^
   "             var_info_map = " ^ var_info_str ^ ";\n" ^
   "             ebp_offset = " ^ ebp_offset_str ^ "; }")

(* for testing only *)
let _mk_block_scope is_arg_scope vi_alist ebp_offset = BlockScope {
    is_arg_scope = is_arg_scope;
    var_info_map = Map.of_alist_exn (module String) vi_alist;
    ebp_offset = ebp_offset;
  }

let empty_bscope ?(is_arg_scope=false) (ebp_offset : int) =
  BlockScope {
    is_arg_scope = is_arg_scope;
    var_info_map = Map.empty (module String);
    ebp_offset = ebp_offset;
  }


let add_obj_to_bscope
    lineno
    (obj: id * annot)
    ?(is_arg_scope=false)
    (BlockScope bscope) =
  (* Make sure the caller knows what they are doing *)
  if not @@ phys_equal bscope.is_arg_scope is_arg_scope
  then failwith "Tried to add local to arguments scope (this is a bug)";
  (* parse the obj *)
  let (Id name, annot) = obj in
  let size = get_obj_size lineno name annot in
  (* See notes above about args vs local vars for why this code works as it
     does: with args we go up in address space and address depends on type size,
     whereas with locals we go down and the address is already known *)
  let cur_ebp_offset = bscope.ebp_offset in
  let new_ebp_offset = cur_ebp_offset + size in
  (* update the ram refs map. Fail (user error) if name already declared *)
  let ram_loc =
    if bscope.is_arg_scope
    then Printf.sprintf  "%d(%%ebp)" @@ new_ebp_offset
    else Printf.sprintf "-%d(%%ebp)" @@ cur_ebp_offset
  in
  let var_info = { ram_loc = ram_loc; annot = annot } in
  let added_var = Map.add bscope.var_info_map ~key:name ~data:var_info in
  let new_var_info_map = (match added_var with
      | `Ok updated_var_info_map ->
        updated_var_info_map
      | `Duplicate ->
        failwith @@ emsg
          lineno
          "Duplicate variable or argument of name " ^ name
    ) in (* return the bscope offset, so that the stack-scope
            code can track the max offset *)
  BlockScope {
    bscope with
    var_info_map = new_var_info_map;
    ebp_offset = new_ebp_offset;
  }


let bscope_from_args lineno (args: (id * annot) list) =
  let initial_bscope = empty_bscope
      initial_args_prev_ebp_offset ~is_arg_scope:true in
  let rec go bscope = function
    | [] -> bscope
    | argvar :: more_args ->
      let new_bscope = add_obj_to_bscope
          lineno argvar bscope
          ~is_arg_scope:true
      in go new_bscope more_args
  in
  go initial_bscope args


let next_bscope_ebp_offset (BlockScope bscope) =
  if bscope.is_arg_scope
  then initial_locals_current_ebp_offset
  else bscope.ebp_offset

(* A Context is what we actually use as we visit the nodes of an Ast.

   It keeps track of both local and global information. The global
   information (not all of it is implemented) has things you might
   need to know about code outside the function:
   - a map of global variables (this would actually form the bottom layer of the
     stack frame scope stack, if we implemented them)
   - a map of functions (this isn't implemented yet as of writing, but we'll
     implement it soon in stage 9)
   - a map of user-defined types (if we implemented them)

   The local information has anything you need to know in order to
   generate code for arbitrary statements inside of the function:
   - everything neeeded to exit the function from a return statement.
     Depending how you implement functions this might not be needed;
     this compiler uses a jump to a function epilogue label rather than
     inlining the epilogue, so we need to know the function name
   - everything needed to handle the loop control statements `break`
     and `continue`. Again what exactly you need could vary; our compiler
     implements all loops using a fixed pattern with labels we can jump
     to, so we just need a stack of label prefixes (a stack so that we
     can handle inner loops; break and continue always refer to the
     innermost surrounding loop)

   Finally, the context (which is an input to expression generation, but
   both an input and an output of statement / declaration generation)
   needs to store any information that statements / blocks / declarations
   might have to pass back up the AST during code generation:
   - function generation needs to know about the esp offset to use, so that
     the operation stack used for expression resolution / fuhction calls
     sits lower in address space than all local variable storage.
     This is why delcaration and statement generation have to return
     a potentially modified Ccontext.

   ------------------------------------------------------------------
   That's all the important stuff. But just a few interesting points:
   ------------------------------------------------------------------
   - the code for generation is pretty messy looking in places because
     of all the explicit context returns; a monadic interface for
     managing the context as state might have worked pretty well. I don't
     know enough ocaml to judge, but in haskell we'd probably do something
     along those lines.
   - there's only one context at a time, and it gets built up as we move
     around the AST. A more complex language might do more interesting
     things:
     - in languages with local functions and types, we might have
       a stack of contexts, or we might create a wrapper type so that
       we could use a single symbol lookup table for functions, types,
       and variables.
     - in a language with fancier syntax, we'd need a single lookup
       table for types, functions, and variables because we wouldn't
       always be able to tell them apart.
     - Also, we might use a context type (without the stack frame information)
       not just in code generation but also in parsing, where as soon as we
       finish parsing some types of AST nodes (e.g. functions / typedefs /
       declarations) we would add them to the context for later nodes.
       We can get away without this in C because the syntax is pretty simple,
       but even our C parser would have been a little easier if we could look up
       a symbol and know whether it is, e.g., a type definition.
       - note that this might also have unintended consequences, e.g. we
         might not be able to parse mutually recursive functions. I suspect
         some issue along these lines may be part of why ocaml requires the
         `and` and `let rec` syntaxes.
     - in languages with first-class functions, functions would probably
       go directly into our stack of scopes as variables

   Finally, what could be better?
   - If I separated the context into function context (stack frame, esp
     offset, function name) vs global context, and made the function
     context an option, it might be cleaner.
*)

type context = Context of {
    args: block_scope option;
    stack_frame: block_scope list;
    esp_offset: int option;
    fn_name: string option;
    current_loop_prefixes: string list;
    fn_map: ( string
            , Ast.annot * (Ast.annot list) * bool
            , String.comparator_witness) Map.t;
  }


let empty_context = Context {
    args = None;
    stack_frame = [];
    esp_offset = None;
    fn_name = None;
    current_loop_prefixes = [];
    fn_map = Map.empty (module String)
  }


let show_context (Context ctx) =
  let stack_frame_str = Util.show_list show_block_scope ctx.stack_frame in
  let offset_str = match ctx.esp_offset with
    | None -> "None"
    | Some i -> "Some \"" ^ (Int.to_string i) ^ "\""
  in
  let fn_name_str = match ctx.fn_name with
    | None -> "None"
    | Some n -> "Some \"" ^ n ^ "\""
  in
  let show_fn_triple (r, a, d) =
    "(" ^ (show_annot r) ^ ", " ^ (Util.show_list show_annot a) ^
    ", " ^ (Bool.to_string d) ^ ")"
  in
  let fn_map_str =
    ("Map.of_alist (module String) " ^
     (Util.show_list
        (fun (f, fi) ->
           Printf.sprintf "(\"%s\", %s)" f (show_fn_triple fi))
        (Map.to_alist ctx.fn_map)))
  in
  ("Context { stack_frame = " ^  stack_frame_str ^ ";\n" ^
   "          esp_offset = " ^ offset_str ^ ";\n" ^
   "          fn_name = " ^ fn_name_str ^ ";\n" ^
   "          fn_map = " ^ fn_map_str ^ "; }")


let ctx_in_fn (Context ctx) =
  let no_name = (phys_equal ctx.fn_name None) in
  let no_esp_offset = (phys_equal ctx.esp_offset None) in
  let no_argument_scope = (Option.is_none ctx.args) in
  let consistent_state =
    ((no_name = no_argument_scope) && (no_name = no_esp_offset)
     || (no_name && ((List.length ctx.stack_frame) > 0)))
  in
  if consistent_state
  then (not no_name)
  else failwith @@ "Had inconsistent context " ^ (show_context (Context ctx))

let ctx_at_toplevel ctx =
  ctx_in_fn (ctx)

let ctx_at_fn_toplevel (Context ctx) =
  (ctx_in_fn (Context ctx)) && ((List.length ctx.stack_frame) = 0)

let ctx_in_fn_body ctx =
  (ctx_in_fn ctx) && (not (ctx_at_fn_toplevel ctx))


let declare_fn
    (Line lineno)
    (fn_name: string)
    (return_type: Ast.annot)
    (arg_types: Ast.annot list)
    (is_defntn: bool)
    (Context ctx) =
  if ctx_in_fn (Context ctx)
  then failwith @@ emsg
      lineno
      "Tried to declare fn when not at top level (compiler bug)"
  else
    (* add a declaration if there is none yet *)
    match Map.find ctx.fn_map fn_name with
    | None ->
      Context {
        ctx with
        fn_map = Map.set ctx.fn_map
            ~key:fn_name ~data:(return_type, arg_types, is_defntn)
      }
    (* otherwise make sure it's consistent, then do nothing *)
    | Some (return_type_, arg_types_, is_defined) ->
      if is_defined && is_defntn
      then failwith @@ emsg
          lineno
          ("Function " ^ fn_name ^ " was defined or declared twice")
      else if not (equal_annot return_type return_type_)
      then failwith @@ emsg
          lineno
          ("Function " ^ fn_name ^ " has inconsistent arg types " ^
           (show_annot return_type) ^ " vs " ^
           (show_annot return_type_))
      else if not (
          (List.length arg_types = List.length arg_types_) &&
          (List.fold2_exn arg_types arg_types_
             ~f:(fun ok a0 a1 -> ok && (equal_annot a0 a1)) ~init:true)
        )
      then failwith @@ emsg
          lineno
          ("Function " ^ fn_name ^ " has inconsistent arg types " ^
           (Util.show_list show_annot arg_types) ^ " vs " ^
           (Util.show_list show_annot arg_types_))
      else Context ctx


let enter_fn
    (Line lineno)
    (fn_name: string)
    (args: (Ast.id * Ast.annot) list)
    (Context ctx) =
  if ctx_in_fn (Context ctx)
  then failwith @@ emsg
      lineno
      "Tried to enter function from not toplevel (this is a bug)"
  else
    let args_bscope = bscope_from_args lineno args in
    Context {
      ctx with
      args = Some args_bscope;
      esp_offset = Some 0;
      fn_name = Some fn_name;
    }

let exit_fn (Context ctx) =
  if not (ctx_at_fn_toplevel (Context ctx))
  then failwith
      "Exited a function when not at function toplevel stack (compiler bug)"
  else
    Context {
      ctx with
      args = None;
      stack_frame = [];
      fn_name = None;
      esp_offset = None;
    }

let enter_block (Context ctx) =
  if not (ctx_in_fn (Context ctx))
  then failwith
      "Tried to enter a block when not in a function (compiler bug)"
  else
    let ebp_offset = (
      match (List.hd ctx.stack_frame) with
      | None ->
        initial_locals_current_ebp_offset
      | Some scope ->
        next_bscope_ebp_offset scope
    ) in
    Context {
      ctx with
      stack_frame = (empty_bscope ebp_offset ) :: ctx.stack_frame
    }

let exit_block (Context ctx) =
  if not (ctx_in_fn_body (Context ctx))
  then failwith
      "Tried to exit a block when there were no block scopes (compiler bug)"
  else
    let outer_bscopes = List.tl_exn ctx.stack_frame in
    Context {
      ctx with
      stack_frame = outer_bscopes
    }

let enter_loop (label_prefix: string) (Context ctx) =
  Context {
    ctx with
    current_loop_prefixes = label_prefix :: ctx.current_loop_prefixes
  }

let exit_loop (Context ctx) =
  let outer_loops = List.tl_exn ctx.current_loop_prefixes in
  Context {
    ctx with
    current_loop_prefixes = outer_loops
  }


let add_var (Line lineno) (var: id * annot) (Context ctx) =
  let (Id objname, _) = var in
  if (
    (* Fail if we're trying to add an arg when not in a function *)
    not (ctx_in_fn_body (Context ctx))
  )
  then failwith @@ emsg
      lineno
      "Tried to add local var from outside a function body (compiler bug)"
  else if (
    (* Fail if we're trying to shadow a function argument *)
    match ctx.args with
    | None ->
      failwith "should not get here"
    | Some (BlockScope { var_info_map = var_map; _ } ) ->
      (
        match Map.find var_map objname with
        | Some _ ->
          true
        | None ->
          false
      )
  )
  then failwith @@ emsg
      lineno
      "Tried to add local var shadowing a function argument"
  else
    (* actually add the var *)
    let old_inner_bscope, outer_bscopes = (
      match ctx.stack_frame with
      | i :: o -> i, o
      | [] -> failwith "should not get here (see guards above)"
    ) in
    let BlockScope inner_bscope = add_obj_to_bscope lineno var old_inner_bscope in
    let stack_frame = (BlockScope inner_bscope) :: outer_bscopes in
    let old_esp_offset = (match ctx.esp_offset with
        | None -> failwith "should not get here (see guards above)"
        | Some offset -> offset
      ) in
    let esp_offset = compute_new_esp_offset
        old_esp_offset inner_bscope.ebp_offset in
    Context {
      ctx with
      stack_frame = stack_frame;
      esp_offset = Some esp_offset;
    }

let get_loop_prefix (Line lineno) (Context ctx) =
  match ctx.current_loop_prefixes with
  | [] ->
    failwith @@ emsg
      lineno
      "Attempted loop control statement (break/continue) outside of loop"
  | current_prefix :: _ ->
    current_prefix


let find_var_info (Line lineno) (Id objname) (Context ctx) =
  let arg_map = match ctx.args with
    | None ->
      failwith @@ emsg
        lineno
        "tried to find var info outside fn context (compiler bug)"
    | Some (BlockScope {var_info_map = var_map; _ }) ->
      var_map
  in
  let rec find_in_scopes = function
    | (BlockScope {var_info_map = var_map; _ }) :: outer_bscopes ->
      (
        match Map.find var_map objname with
        | Some ram_reference ->
          ram_reference
        | None ->
          find_in_scopes outer_bscopes
      )
    | [] ->
      (
        match Map.find arg_map objname with
        | Some ram_reference ->
          ram_reference
        | None ->
          failwith @@ emsg
            lineno
            "Could not find object " ^ objname ^ " in current scope" 
      )
  in find_in_scopes ctx.stack_frame


let find_ram_loc line the_id context =
  let info = find_var_info line the_id context in
  info.ram_loc


let find_type_annot line the_id context =
  let info = find_var_info line the_id context in
  info.annot


let find_fn_info (Line lineno) fname (Context context) =
  let search = Map.find context.fn_map fname in
  match search with
  | Some (ret, args, _) ->
    (ret, args)
  | None ->
    failwith @@ emsg
      lineno
      "Could not find function with name " ^ fname
