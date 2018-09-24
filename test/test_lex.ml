open OUnit2
open Tnqcc

let lex_tcase code expected_tokens =
  let actual_tokens = lex code in
  (fun _ ->
     assert_equal expected_tokens actual_tokens) (* ~printer:tokens_to_string*)

let tests = [
    "can lex some simple tokens" >:: lex_tcase
      "() { ; }"
      Tnqcc.Tok.([LPAREN; RPAREN; LBRACE; SEMICOL; RBRACE;]);
    "ignores whitespace properly" >:: lex_tcase
      "{ \t\t\n ; \r\n\r\t\n }"
      Tnqcc.Tok.([LBRACE; SEMICOL; RBRACE;]);
    "lexes keywords and identifiers" >:: lex_tcase
      "int x; return x;"
      Tnqcc.Tok.([
          KW_INT; ID "x"; SEMICOL;
          KW_RETURN; ID "x"; SEMICOL;
        ]);
    "lexes literals" >:: lex_tcase
      "return 15;"
      Tnqcc.Tok.([
          KW_RETURN; LIT_INT 15; SEMICOL;
        ]);
    "lexes a simple function" >:: lex_tcase
      "int main() { return 0; }"
      Tnqcc.Tok.([
          KW_INT; ID "main"; LPAREN; RPAREN;
          LBRACE; KW_RETURN; LIT_INT 0; SEMICOL; RBRACE;
        ]);
  ]
