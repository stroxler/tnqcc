open Base
open Ast


let emsg (line: line) (context: string) (display: string) =
  let Line lineno = line in
  Printf.sprintf "Could not generate at line %d\nContext: %s\nAst: %s"
                 lineno context display

let rec gen_block body =
  String.concat @@ List.map ~f:gen_block_item body

and gen_block_item = function
  | Statement (s, l) -> gen_statement l s
  | Definition (d, l) -> failwith @@ emsg l "gen_block_item" @@ show_def_var d

and gen_statement l s = match s with
  | Return (Lit (Int n)) ->
    let mov_cmd = Printf.sprintf "\tmovl $%d, %s" n "%eax" in
    let ret_cmd = "\tret" in
    mov_cmd ^ "\n" ^ ret_cmd
  | bad -> failwith @@ emsg l "gen_statement" @@ show_statement bad

(* Linux and osx have different function label conventions *)
let gen_fn = function
  | DefFn {annot=_; name=Id name; body=Some body; line=_} ->
    let mangledName =
      if Util.equal_os_type Util.current_os Util.Linux
      then name
      else "_" ^ name
    in
    let decl = Printf.sprintf "\t.globl %s" mangledName in
    let label = Printf.sprintf "%s:" mangledName in
    decl ^ "\n" ^ label ^ "\n" ^ (gen_block body) ^ "\n\n"
  | DefFn {annot=_; name=_; body=None; line=_; } ->
    ""

let gen = function
  | Prog fns -> String.concat @@ List.map ~f:gen_fn fns

