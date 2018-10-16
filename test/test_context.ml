open OUnit2
open Tnqcc
open Tnqcc.Gen.Context


(* Comparing strings is a nasty hack here, because
   I get weird errors (Invalid_Argument"compare: functional value")
   otherwise ... I'm guessing the underlying issue is with Map.t
   not implementing the interface for physical equality *)
let context_tcase (actual:context) (expected:context) =
  fun _ ->
    assert_equal
      (show_context expected)
      (show_context actual)
      ~printer:(fun s -> s)


let c0_actual = empty_context
let c0_expected = Context {
    stack_frame = [];
    esp_offset = -999;
    fn_name = None;
    current_loop_prefixes = [];
  }
let test_c0 =
  "empty_context is as expected" >:: context_tcase
    c0_actual c0_expected

let c1_actual = (new_function (Line 5) "myfunc" [] c0_expected)
let c1_expected = Context {
    stack_frame = [
      _mk_block_scope true [] 4;
    ];
    esp_offset = 0;
    fn_name = Some "myfunc";
    current_loop_prefixes = [];
  }
let test_c1 =
  "new_function behaves as expected" >:: context_tcase
    c1_actual c1_expected

let c1_5 = enter_block c1_expected
let c2_actual = add_var (Line 6) (Ast.Id "x", Ast.Annot "int") c1_5
let c2_expected = (
  let total_space_for_locals = 1 * word_size in
  let args_scope = (empty_bscope word_size ~is_arg_scope:true) in
  let top_scope = (
    _mk_block_scope
      false
      [
        ("x", {ram_loc = "-4(%ebp)"; annot = Ast.Annot "int"});
      ]
      (word_size + total_space_for_locals)
  ) in
  Context {
    stack_frame = [ top_scope; args_scope ];
    esp_offset = total_space_for_locals;
    fn_name = Some "myfunc";
    current_loop_prefixes = [];
  })
let test_c2 =
  "add_var behaves as expected if we add 1 var" >:: context_tcase
    c2_actual c2_expected

let c3_actual = add_var (Line 6) (Ast.Id "y", Ast.Annot "int") c2_expected
let c3_expected = (
  let total_space_for_locals = 2 * word_size in
  let args_scope = (empty_bscope word_size ~is_arg_scope:true) in
  let scope_1 = (
    _mk_block_scope
      false
      [
        ("x", {ram_loc = "-4(%ebp)"; annot = Ast.Annot "int"});
        ("y", {ram_loc = "-8(%ebp)"; annot = Ast.Annot "int"});
      ]
      (word_size + total_space_for_locals)
  ) in
  Context {
    stack_frame = [ scope_1; args_scope ];
    esp_offset = total_space_for_locals;
    fn_name = Some "myfunc";
    current_loop_prefixes = [];
  })
let test_c3 =
  "add_var behaves as expected if we add 2 vars" >:: context_tcase
    c3_actual c3_expected

let c3_5 = enter_block c3_expected
let c4_actual = add_var (Line 8) (Ast.Id "x", Ast.Annot "int") c3_5
let c4_expected = (
  let total_space_1 = 2 * word_size in
  let total_space_2 = 1 * word_size in
  let total_space_for_locals = total_space_1 + total_space_2 in
  let args_scope = (empty_bscope word_size ~is_arg_scope:true) in
  let scope_1 = (
    _mk_block_scope
      false
      [
        ("x", {ram_loc = "-4(%ebp)"; annot = Ast.Annot "int"});
        ("y", {ram_loc = "-8(%ebp)"; annot = Ast.Annot "int"});
      ]
      (word_size + total_space_1)
  ) in
  let scope_2 = (
    _mk_block_scope
      false
      [
        ("x", {ram_loc = "-12(%ebp)"; annot = Ast.Annot "int"});
      ]
      (word_size + total_space_1 + total_space_2)
  ) in
  Context {
    stack_frame = [ scope_2; scope_1; args_scope ];
    esp_offset = total_space_for_locals;
    fn_name = Some "myfunc";
    current_loop_prefixes = [];
  }
)
let test_c4 =
  "add_var behaves as expected if we add another scope scope" >:: context_tcase
    c4_actual c4_expected

let c5_actual = exit_block c4_expected
let c5_expected =
  let Context ctx3 = c3_expected in
  let Context ctx4 = c4_expected in
  Context { ctx3 with esp_offset = ctx4.esp_offset }
let test_c5 =
  "exit_block behaves as expected" >:: context_tcase
    c5_actual c5_expected


let c6_actual = enter_loop "outer" (enter_loop "inner" c5_expected)
let c6_expected =
  let Context ctx5 = c5_expected in
  Context { ctx5 with current_loop_prefixes = ["outer"; "inner"]; }
let test_c6 =
  "enter_loop behaves as expected" >:: context_tcase
    c6_actual c6_expected

let c7_actual = exit_loop c6_expected
let c7_expected =
  let Context ctx5 = c5_expected in
  Context { ctx5 with current_loop_prefixes = ["inner"]; }
let test_c7 =
  "exit_loop behaves as expected" >:: context_tcase
    c7_actual c7_expected

let tests = [

  test_c0;
  test_c1;
  test_c2;
  test_c3;
  test_c4;
  test_c5;
  test_c6;
  test_c7;

  "find_info behaves as expected"  >:: fun _ ->
    (
      let x_info_c2 = find_info (Ast.Line 3) (Ast.Id "x") c3_expected in
      assert_equal
        { ram_loc = "-4(%ebp)"; annot = Ast.Annot "int" }
        x_info_c2
        ~printer:show_var_info
    );
    (
      let x_info = find_info (Ast.Line 3) (Ast.Id "x") c4_expected in
      assert_equal
        { ram_loc = "-12(%ebp)"; annot = Ast.Annot "int" }
        x_info
        ~printer:show_var_info
    );
    (
      let y_info = find_info (Ast.Line 3) (Ast.Id "y") c4_expected in
      assert_equal
        { ram_loc = "-8(%ebp)"; annot = Ast.Annot "int" }
        y_info
        ~printer:show_var_info
    );
    (
      let y_info = find_info (Ast.Line 3) (Ast.Id "y") c5_expected in
      assert_equal
        { ram_loc = "-8(%ebp)"; annot = Ast.Annot "int" }
        y_info
        ~printer:show_var_info
    );
    ;

  "get_loop_prefix behaves as expected"  >:: fun _ ->
    (
      let loop_prefix = get_loop_prefix (Line 1) c6_expected in
      assert_equal
        "outer"
        loop_prefix
        ~printer:(fun s -> s)
    );
    (
      let loop_prefix = get_loop_prefix (Line 1) c7_expected in
      assert_equal
        "inner"
        loop_prefix
        ~printer:(fun s -> s)
    );
    ;
]
