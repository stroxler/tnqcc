(*
   The AST for our subset of C

   As of this commit, we should be able to parse this
   minimal program:

   ```
   int main() {
       return 2;
   }
   ```
   *)

type line = Line of int
  [@@deriving show]

(* literal values *)
type lit =
  | Int of int
  [@@deriving show]

(* identifiers (variables, functions) *)
type id = Id of string
  [@@deriving show]

type binary_op =
  | Add
  | Sub
  | Mult
  | Div
  | Mod
  | LAnd
  | LOr
  | BAnd
  | BOr
  | XOr
  | Eq
  | Neq
  | Leq
  | Geq
  | Lt
  | Gt
  [@@deriving show, eq]

type unary_op =
  | LNot
  | BNot
  | Neg
  [@@deriving show]

(* expressions (evaluate to values) *)
type expr =
  | Lit of lit
  | UnaryOp of unary_op * expr
  | BinaryOp of binary_op * expr * expr
  [@@deriving show]

(* type annotations (used in definitions) *)
type annot =
  | IntAnnot
  [@@deriving show]

(* variable definitions *)
type def_var = DefVar of {
    annot: annot;
    name: id;
    init: expr option;
  }
  [@@deriving show]

(* statements and block items. Note the need for recursive declarations *)
type statement =
  | Block of block
  | Return of expr
  [@@deriving show]

and block =
  block_item list
  [@@deriving show]

(* block items are the lowest-level thing with a line number *)
and block_item =
  | Statement of statement * line
  | Definition of def_var * line
  [@@deriving show]


(* function definitions - these have a line number *)
type def_fn = DefFn of {
    annot: annot;
    name: id;
    (* TODO add params *)
    body: block option;
    line: line;
  }
  [@@deriving show]

(* full program *)
type prog = Prog of def_fn list
  [@@deriving show]
