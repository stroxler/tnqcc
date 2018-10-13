open OUnit2
open Tnqcc.Tok

let lex_tcase code expected_tokens =
  let actual_tokens = Tnqcc.Lex.lex_string code in
  (fun _ ->
     assert_equal expected_tokens actual_tokens ~printer:show_tokens)


let tests = [
  "can lex some simple tokens" >:: lex_tcase
    "() { ; }"
    [
      (LPAREN,  1);
      (RPAREN,  1);
      (LBRACE,  1);
      (SEMICOL, 1);
      (RBRACE,  1);
      (EOF,     1);
    ];
  "ignores whitespace properly" >:: lex_tcase
    "{ \t\t\n ; \t\n\t\t\r\n }"
    [
      (LBRACE,  1);
      (SEMICOL, 2);
      (RBRACE,  4);
      (EOF,     4);
    ];
  "lexes keywords and identifiers" >:: lex_tcase
    "int x; return x;"
    [
      (KW_INT,    1);
      (ID "x",    1);
      (SEMICOL,   1);
      (KW_RETURN, 1);
      (ID "x",    1);
      (SEMICOL,   1);
      (EOF,       1);
    ];
  "lexes literals" >:: lex_tcase
    "return 15;"
    [
      (KW_RETURN,  1);
      (LIT_INT 15, 1);
      (SEMICOL,    1);
      (EOF,        1);
    ];
  "lexes unary operators" >:: lex_tcase
    "-1; ~15; !0; ^16;"
    [
      (OP_MINUS,   1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (OP_TILDE,   1);
      (LIT_INT 15, 1);
      (SEMICOL,    1);
      (OP_BANG,    1);
      (LIT_INT 0,  1);
      (SEMICOL,    1);
      (OP_XOR,     1);
      (LIT_INT 16, 1);
      (SEMICOL,    1);
      (EOF,        1);
    ];
  "lexes more keywords" >:: lex_tcase
    "if else return continue break do while for"
    [
      (KW_IF,       1);
      (KW_ELSE,     1);
      (KW_RETURN,   1);
      (KW_CONTINUE, 1);
      (KW_BREAK,    1);
      (KW_DO,       1);
      (KW_WHILE,    1);
      (KW_FOR,      1);
      (EOF,        1);
    ];
  "lexes binary operators" >:: lex_tcase
    "1+1; 1*1; 1& 1; 1 &&1; 1|1; 1 || 1; 1%1; 1 /1;"
    [
      (LIT_INT 1,  1);
      (OP_PLUS,    1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (LIT_INT 1,  1);
      (OP_STAR,    1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (LIT_INT 1,  1);
      (OP_SAND,    1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (LIT_INT 1,  1);
      (OP_DAND,    1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (LIT_INT 1,  1);
      (OP_SOR,     1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (LIT_INT 1,  1);
      (OP_DOR,     1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (LIT_INT 1,  1);
      (OP_MOD,     1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (LIT_INT 1,  1);
      (OP_DIV,     1);
      (LIT_INT 1,  1);
      (SEMICOL,    1);
      (EOF,        1);
    ];
  "lexes a simple function" >:: lex_tcase
    "int main() { return 0; }"
    [
      (KW_INT,    1);
      (ID "main", 1);
      (LPAREN,    1);
      (RPAREN,    1);
      (LBRACE,    1);
      (KW_RETURN, 1);
      (LIT_INT 0, 1);
      (SEMICOL,   1);
      (RBRACE,    1);
      (EOF,       1);
    ];
  "lexes two functions" >:: lex_tcase
    "int main() { return 0; }\nint cool() { return 1; }"
    [
      (KW_INT,    1);
      (ID "main", 1);
      (LPAREN,    1);
      (RPAREN,    1);
      (LBRACE,    1);
      (KW_RETURN, 1);
      (LIT_INT 0, 1);
      (SEMICOL,   1);
      (RBRACE,    1);
      (KW_INT,    2);
      (ID "cool", 2);
      (LPAREN,    2);
      (RPAREN,    2);
      (LBRACE,    2);
      (KW_RETURN, 2);
      (LIT_INT 1, 2);
      (SEMICOL,   2);
      (RBRACE,    2);
      (EOF,       2);
    ];
]
