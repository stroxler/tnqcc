open OUnit2
open Tnqcc.Ast


let prog_tcase code expected_ast =
  let actual_ast = Tnqcc.parse @@ Tnqcc.Lex.lex_string code in
  (fun _ ->
     assert_equal expected_ast actual_ast ~printer:show_prog)

let expr_tcase code expected_expr =
  let full_code = "int main () { return " ^ code ^ " ; }" in
  let epxected_ast = (Prog [
      DefFn {
        annot = Annot "int"; name = Id "main";
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
    ])
  in prog_tcase full_code epxected_ast

let block_item_tcase code expected_item =
  let full_code = "int main () { " ^ code ^ " return 0; }" in
  let epxected_ast = (Prog [
      DefFn {
        annot = Annot "int"; name = Id "main";
        body = Some [
            expected_item;
            Statement (
              Return (
                Lit (Int 0)
              ),
              Line 1
            );
          ];
        line = Line 1;
      };
    ])
  in prog_tcase full_code epxected_ast



let tests = [

  "can parse a single, simple function" >:: prog_tcase
    "int main() \n { \t return 0; \n } \n"
    (
      Prog [
        DefFn {
          annot = Annot "int"; name = Id "main";
          body = Some [
              Statement (Return (Lit (Int 0)), Line 2);
            ];
          line = Line 1;
        };
      ]
    );

  "can parse multiple functions, including user-defined types" >:: prog_tcase
    "int main() { return 55; }\n\n my_t cool() { return 10; }"
    (
      Prog [
        DefFn {
          annot = Annot "int"; name = Id "main";
          body = Some [
              Statement (Return (Lit (Int 55)), Line 1);
            ];
          line = Line 1;
        };
        DefFn {
          annot = Annot "my_t"; name = Id "cool";
          body = Some [
              Statement (Return (Lit (Int 10)), Line 3);
            ];
          line = Line 3;
        };
      ]
    );

  "can parse variable definitions with no initializer" >:: block_item_tcase
    "int x;"
    (
      Definition (
        DefVar {
          annot = Annot "int";
          id = Id "x";
          init = None;
        },
        Line 1
      )
    );

  "can parse variable definitions with user-defined types" >:: block_item_tcase
    "my_t x;"
    (
      Definition (
        DefVar {
          annot = Annot "my_t";
          id = Id "x";
          init = None;
        },
        Line 1
      )
    );

  "can parse variable definitions with an initializer" >:: block_item_tcase
    "int x = 1 + 1;"
    (
      Definition (
        DefVar {
          annot = Annot "int";
          id = Id "x";
          init = Some (BinaryOp (Add, Lit (Int 1), Lit (Int 1)));
        },
        Line 1
      )
    );

  "can parse conditional statements with no else" >:: block_item_tcase
    "if (x == 2) y = 7;"
    (
      Statement (
        Conditional (
          BinaryOp (Eq, Reference (Id "x"), Lit (Int 2)),
          Expr ( Some ( Assign (
              Id "y",
              Lit (Int 7)
            ))),
          Expr None
        ),
        (Line 1)
      )
    );

  "can parse conditional statements with an else" >:: block_item_tcase
    "if (x == 2) x = 5; else y = 7;"
    (
      Statement (
        Conditional (
          BinaryOp (Eq, Reference (Id "x"), Lit (Int 2)),
          Expr ( Some ( Assign ((Id "x"), Lit (Int 5)))),
          Expr ( Some ( Assign (
              Id "y",
              Lit (Int 7)
            )))
        ),
        (Line 1)
      )
    );

  "can parse blocks" >:: block_item_tcase
    "{ int x; x = 5; y + 7; }"
    (
      Statement (
        Block [
          Definition (
            DefVar {
              annot = Annot "int";
              id = Id "x";
              init = None;
            },
            Line 1
          );
          Statement (
            Expr ( Some ( Assign ((Id "x"), Lit (Int 5)))),
            Line 1
          );
          Statement (
            Expr ( Some ( BinaryOp (Add, Reference (Id "y"), Lit (Int 7)))),
            Line 1
          );
        ],
        Line 1
      )
    );

  "can parse for loops with declarations" >:: block_item_tcase
    "for (int i = 0; i < 10; i = i + 1) ;"
    (
      Statement
        ( ForLoopDecl
            ( DefVar { annot = Annot "int"
                     ; id = Id "i"
                     ; init = Some (Lit (Int 0)) }
            , Some ( BinaryOp ( Lt, Reference (Id "i"), Lit (Int 10)) )
            , Some
                ( Assign ( Id "i"
                         , BinaryOp ( Add, Reference (Id "i"), Lit (Int 1))
                         )
                )
            , Expr None
            )
        , (Line 1)
        )
    );

  "can parse for loops with starting expression / break statements" >:: block_item_tcase
    "for (i = 0; i < 10; ) break;"
    (
      Statement
        ( ForLoop
            ( Some
                ( Assign ( Id "i"
                         , Lit (Int 0)
                         )
                )
            , Some ( BinaryOp ( Lt, Reference (Id "i"), Lit (Int 10)) )
            , None
            , Break
            )
        , (Line 1)
        )
    );

  "can parse for loops empty for / continue statements" >:: block_item_tcase
    "for ( ; ; ) continue;"
    (
      Statement
        ( ForLoop
            ( None
            , None
            , None
            , Continue
            )
        , (Line 1)
        )
    );

  "can parse while loops" >:: block_item_tcase
    "while (i < 10) ;"
    (
      Statement
        ( WhileLoop
            ( BinaryOp ( Lt, Reference (Id "i"), Lit (Int 10))
            , Expr None
            )
        , Line 1
        )
    );

  "can parse do-while loops" >:: block_item_tcase
    "do ; while (i < 10);"
    (
      Statement
        ( DoLoop
            ( Expr None
            , BinaryOp ( Lt, Reference (Id "i"), Lit (Int 10))
            )
        , Line 1
        )
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

  (* the desired precedence here is higher than unary operations, but
     lower than atoms. Obviously it's not a great idea to actually write C
     code like this, but we do want to check the logic *)
  "can parse assignment operators, with the right precedence" >:: expr_tcase
    "1 + -(x = 5 +5)"
    (
      BinaryOp (
        Add,
        Lit (Int 1),
        UnaryOp (
          Neg,
          Assign (
            Id "x",
            BinaryOp (Add, Lit (Int 5), Lit (Int 5))
          )
        )
      )
    );

  "can parse trinary operators" >:: expr_tcase
    "x ? y = z : 5"
    (
      TrinaryOp (
        Reference (Id "x"),
        Assign ((Id "y"), Reference (Id "z")),
        Lit (Int 5)
      )
    );
]
