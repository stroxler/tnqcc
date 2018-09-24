open Base
open Tok


(* TODO extend this to handle hex integers *)
let int_re = Str.regexp_case_fold
    "\\([0-9]+\\)\\(\\b.*\\)"

let id_or_kw_re = Str.regexp
    "\\([A-Za-z_][A-Za-z0-9_]*\\)\\(\\b.*\\)"

let id_or_kw_token str =
  match str with
  | "return" -> KW_RETURN
  | "int" -> KW_INT
  | _ -> ID str


let lex_complex_token code =
  let str_code = String.of_char_list code in
  if Str.string_match id_or_kw_re str_code 0
  then (* it is an id or a keyword *)
    let id_str = Str.matched_group 1 str_code in
    let rest_str = Str.matched_group 2 str_code in
    let tok = id_or_kw_token id_str in
    tok, String.to_list rest_str
  else if Str.string_match int_re str_code 0
  then (* it is an int literal *)
    let int_str = Str.matched_group 1 str_code in
    let rest_str = Str.matched_group 2 str_code in
    let int_val = Int.of_int32 @@ Int32.of_string @@ int_str in
    match int_val with
    | None -> failwith ("Could not parse " ^ int_str ^ " as an int")
    | (Some i) -> LIT_INT i, String.to_list rest_str
  else
    failwith ("Syntax error: \"" ^ str_code ^ "\" is invalid")


let rec lex_rev code tokens =
  match code with
  | [] -> tokens
  | '{'::rest -> lex_rev rest (LBRACE::tokens)
  | '}'::rest -> lex_rev rest (RBRACE::tokens)
  | '('::rest -> lex_rev rest (LPAREN::tokens)
  | ')'::rest -> lex_rev rest (RPAREN::tokens)
  | ';'::rest -> lex_rev rest (SEMICOL::tokens)
  | c::rest ->
    if Char.is_whitespace c
    then lex_rev rest tokens
    else
      let tok, rest = lex_complex_token code in
      lex_rev rest (tok::tokens)

let lex code =
  let code_list = Base.String.to_list code in
  let tokens_rev = lex_rev code_list [] in
  List.rev tokens_rev
