open Base
open Ast


let emsg (context: string) (display: string) =
  Printf.sprintf "Could not generate code (in %s) for %s" context display

let rec gen_block body =
  String.concat @@ List.map ~f:gen_block_item body

and gen_block_item = function
  | Statement s -> gen_statement s
  | bad -> failwith @@ emsg "gen_block_itm" @@ show_block_item bad

and gen_statement = function
  | Return (Lit (Int n)) ->
    let mov_cmd = Printf.sprintf "\tmovl $%d, %s" n "%eax" in
    let ret_cmd = "\tret" in
    mov_cmd ^ "\n" ^ ret_cmd
  | bad -> failwith @@ emsg "gen_statement" @@ show_statement bad

let gen_fn = function
  | DefFn {annot=_; name=Id name; body=Some body} ->
    let decl = Printf.sprintf "\t.globl %s" name in
    let label = Printf.sprintf "%s:" name in
    decl ^ "\n" ^ label ^ "\n" ^ (gen_block body) ^ "\n\n"
  | DefFn {annot=_; name=_; body=None} ->
    ""

let gen = function
  | Prog fns -> String.concat @@ List.map ~f:gen_fn fns

