type token =
  (* having an explicit EOF token isn't really needed for a
     functional compiler if we don't care about error messages,
     but it makes parsing with line numbers on errors much easier
     because you can always assume there's a next token and
     corresponding line number *)
  | EOF
  (* delimiter symbols *)
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | SEMICOL
  (* keywords *)
  | KW_INT
  | KW_RETURN
  (* literals *)
  | LIT_INT of int
  (* user-defined things *)
  | ID of string
  [@@deriving show]

type pair = token * int
  [@@deriving show]

type t = pair

let show_tokens = Util.show_list show_pair
