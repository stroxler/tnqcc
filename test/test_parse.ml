open OUnit2
open Tnqcc
open Ast


let parse_tcase code expected_ast =
  let actual_ast = parse @@ lex code in
  (fun _ ->
     assert_equal expected_ast actual_ast ~printer:Ast.show_prog)

let tests = [
  "can parse a single, simple function" >:: parse_tcase
    "int main() \n { \t return 0; \n } \n"
    (Prog [
        DefFn {
          annot = IntAnnot; name = Id "main";
          body = Some [
              Statement (Return (Lit (Int 0)));
            ]
        };
      ]);
  "can parse multiple simple functions" >:: parse_tcase
    "int main() { return 55; }\n\n int cool() { return 10; }"
    (Prog [
        DefFn {
          annot = IntAnnot; name = Id "main";
          body = Some [
              Statement (Return (Lit (Int 55)));
            ]
        };
        DefFn {
          annot = IntAnnot; name = Id "cool";
          body = Some [
              Statement (Return (Lit (Int 10)));
            ]
        };
      ]);
]
