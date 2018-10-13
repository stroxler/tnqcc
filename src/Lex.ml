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


let lex_complex_token lineno code =
  let str_code = String.of_char_list code in
  (* split the code string into the current line and future line
      the last regexp group won't match past a newline, which is why we need this logic *)
  let rest_of_chars (rest_of_line: string) = String.to_list @@ rest_of_line in
  (* check for each of the possible regexp matches *)
  if Str.string_match id_or_kw_re str_code 0
  then (* it is an id or a keyword *)
    let match_str = Str.matched_group 1 str_code in
    let rest_str = Str.matched_group 2 str_code in
    let tok = id_or_kw_token match_str in
    tok, rest_of_chars rest_str
  else if Str.string_match int_re str_code 0
  then (* it is an int literal *)
    let match_str = Str.matched_group 1 str_code in
    let rest_str = Str.matched_group 2 str_code in
    let int_val = Int.of_int32 @@ Int32.of_string @@ match_str in
    match int_val with
    | None -> failwith ("Lex error at line " ^ Int.to_string lineno ^
                        "... could not parse " ^ match_str ^ " as an int")
    | (Some i) -> LIT_INT i, rest_of_chars rest_str
  else
    failwith ("Syntax error at line " ^ Int.to_string lineno ^
              ": \"" ^ str_code ^ "\" is invalid")


let rec go_lex lineno code tokens =
  match code with
  | [] -> List.rev tokens
  | '{'::rest -> go_lex lineno rest (LBRACE::tokens)
  | '}'::rest -> go_lex lineno rest (RBRACE::tokens)
  | '('::rest -> go_lex lineno rest (LPAREN::tokens)
  | ')'::rest -> go_lex lineno rest (RPAREN::tokens)
  | ';'::rest -> go_lex lineno rest (SEMICOL::tokens)
  | c::more ->
    if Char.is_whitespace c
    then go_lex lineno more tokens
    else
      let tok, rest = lex_complex_token lineno code in
      go_lex lineno rest (tok::tokens)


let lex_line (lineno_zeroindexed: int) (code: string) =
  let lineno = lineno_zeroindexed + 1 in
  let code_list = Base.String.to_list code in
  let tokens = go_lex lineno code_list [] in
  List.map ~f:(fun tok -> tok, lineno) tokens


let lex_lines (lines: string list) =
  let all_but_last = List.concat @@ List.mapi ~f:lex_line lines in
  let last_line = List.length lines in
  let last = [(EOF, last_line)] in
  List.append all_but_last last


let lex_string (code : string) =
  lex_lines @@ String.split_lines code

let lex_chan inx =
  lex_lines @@ Core.In_channel.input_lines inx
