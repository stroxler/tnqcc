open Core

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
  (* operators (unary vs binary does not matter for lexing,
     although it *does* matter that operators which are
     prefixes of others get handled later in our matching. *)
  | OP_DEQ  (* double equal *)
  | OP_NEQ  (* not equal *)
  | OP_SEQ  (* single equal (assign) *)
  | OP_GEQ
  | OP_LEQ
  | OP_GT
  | OP_LT
  | OP_BANG
  | OP_PLUS
  | OP_MINUS
  | OP_STAR
  | OP_DIV
  | OP_MOD
  | OP_DAND   (* logical and ... *)
  | OP_DOR    (* ... and or *)
  | OP_SAND   (* various bitwise boolean ops *)
  | OP_SOR
  | OP_TILDE  (* ~ is bitwise complement *)
  | OP_XOR
  | OP_QUESTION
  | OP_COLON
  (* keywords *)
  | KW_RETURN
  | KW_INT
  | KW_CHAR
  | KW_IF
  | KW_ELSE
  | KW_FOR
  | KW_DO
  | KW_WHILE
  | KW_BREAK
  | KW_CONTINUE
  (* literals *)
  | LIT_INT of int
  (* user-defined things *)
  | ID of string
  [@@deriving show, ord, sexp]

type pair = token * int
  [@@deriving show]

type t = pair
type tokens = pair list
let show_tokens = Util.show_list show_pair


module Token = struct
  module T = struct
    type t = token
    let compare = compare_token
    let sexp_of_t = sexp_of_token
  end
  include T
  include Base.Comparator.Make(T)
end
