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

(* literal values *)
type lit =
  | Int of int
  [@@deriving show]

(* identifiers (variables, functions) *)
type id = Id of string
  [@@deriving show]

(* expressions (evaluate to values) *)
type expr =
  | Lit of lit
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

(* statements *)
type statement =
  | Block of block
  | Return of expr
  [@@deriving show]

and block =
  block_item list
  [@@deriving show]

and block_item =
  | Statement of statement
  | Definition of def_var
  [@@deriving show]


(* function definitions *)
type def_fn = DefFn of {
    annot: annot;
    name: id;
    (* TODO add params *)
    body: block option;
  }
  [@@deriving show]

(* full program *)
type prog = Prog of def_fn list
  [@@deriving show]
