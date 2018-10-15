open Core

module Tok = Tok
module Ast = Ast

module Parse = Parse
module Lex = Lex
module Gen = Gen

let lex_chan   x = Lex.lex_chan   x |> Tok.show_tokens

let parse = Parse.parse
let parse_chan   x = Lex.lex_chan    x |> parse |> Ast.show_prog

let gen = Gen.gen
let gen_string s = Lex.lex_string s |> parse |> gen
let gen_chan   x = Lex.lex_chan    x |> parse |> gen


(* Note: the optional arguments cannot be last, because
   of currying rules *)
let run
    ?(in_path: string = "-")
    ?(out_path: string = "-")
    (cmd: Core. In_channel.t -> string) =
  let inx = match in_path with
    | "-" -> Core.In_channel.stdin
    | filename -> Core.In_channel.create filename
  in
  let result = cmd inx in
  let outx = match out_path with
    | "-" -> Core.Out_channel.stdout
    | filename -> Core.Out_channel.create filename
  in
  Core.Out_channel.output_string outx result
