open OUnit2
open Tnqcc.Ast


let prog_tcase code expected_ast =
  let actual_ast = Tnqcc.parse @@ Tnqcc.Lex.lex_string code in
  (fun _ ->
     assert_equal expected_ast actual_ast ~printer:show_prog)

let expr_tcase code expected_expr =
  let full_code = "int main () { return " ^ code ^ " ; }" in
  let epxected_ast = Prog [
      DefFn {
        annot = IntAnnot; name = Id "main";
        body = Some [
            Statement (
              Return (
                expected_expr
              ),
              Line 1
            );
          ];
        line = Line 1;
      };
    ]
  in prog_tcase full_code epxected_ast




let tests = [
  "can parse a single, simple function" >:: prog_tcase
    "int main() \n { \t return 0; \n } \n"
    (
      Prog [
        DefFn {
          annot = IntAnnot; name = Id "main";
          body = Some [
              Statement (Return (Lit (Int 0)), Line 2);
            ];
          line = Line 1;
        };
      ]
    );
  "can parse multiple simple functions" >:: prog_tcase
    "int main() { return 55; }\n\n int cool() { return 10; }"
    (
      Prog [
        DefFn {
          annot = IntAnnot; name = Id "main";
          body = Some [
              Statement (Return (Lit (Int 55)), Line 1);
            ];
          line = Line 1;
        };
        DefFn {
          annot = IntAnnot; name = Id "cool";
          body = Some [
              Statement (Return (Lit (Int 10)), Line 3);
            ];
          line = Line 3;
        };
      ]
    );
  "can parse unary operators" >:: expr_tcase
    "-!~1"
    (
      UnaryOp (
        Neg,
        (UnaryOp (LNot, (UnaryOp (BNot, Lit (Int 1)))))
      )
    );
  "can parse binary operators, part 1" >:: expr_tcase
    "((1 +2  *3))"
    (
      BinaryOp (
        Add,
        Lit (Int 1),
        BinaryOp (Mult, Lit (Int 2), Lit (Int 3))
      )
    );
  "can parse binary operators, part 2" >:: expr_tcase
    "((1 +2)  *3)"
    (
      BinaryOp (
        Mult,
        BinaryOp (Add, Lit (Int 1), Lit (Int 2)),
        Lit (Int 3)
      )
    );
  "can parse binary operators, part 3" >:: expr_tcase
    "0 || 1 & 5 && (1 +2)"
    (
      BinaryOp (
        LOr,
        Lit (Int 0),
        BinaryOp (
          LAnd,
          BinaryOp (BAnd, Lit (Int 1), Lit (Int 5)),
          BinaryOp (Add, Lit (Int 1), Lit (Int 2))
        )
      )
    );
  "is left-associative on binary operators" >:: expr_tcase
    "1 % 2 % 3"
    (
      BinaryOp (
        Mod,
        BinaryOp (
          Mod,
          Lit (Int 1),
          Lit (Int 2)
        ),
        Lit (Int 3)
      )
    );
  "can parse mixed binary and unary ops" >:: expr_tcase
    "((1 +2)  *-3)"
    (
      BinaryOp (
        Mult,
        BinaryOp (Add, Lit (Int 1), Lit (Int 2)),
        UnaryOp (Neg, Lit (Int 3))
      )
    );
]
