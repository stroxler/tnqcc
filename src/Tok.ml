type token =
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

type t = token

let show_tokens = Util.show_list show_token
