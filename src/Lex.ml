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
  match String.split_lines @@ String.of_char_list code with
  | str_code::later_line_strings ->
    (* split the code string into the current line and future lines
       the last regexp group won't match past a newline, which is why we need this logic *)
    let later_lines = String.concat ~sep:"\n" later_line_strings in
    let rest_of_chars (rest_of_line: string) = String.to_list @@ rest_of_line ^ later_lines in
    (* check for each of the possible regexp matches *)
    if Str.string_match id_or_kw_re str_code 0
    then (* it is an id or a keyword *)
      let id_str = Str.matched_group 1 str_code in
      let rest_str = Str.matched_group 2 str_code in
      let tok = id_or_kw_token id_str in
      tok, rest_of_chars rest_str
    else if Str.string_match int_re str_code 0
    then (* it is an int literal *)
      let int_str = Str.matched_group 1 str_code in
      let rest_str = Str.matched_group 2 str_code in
      let int_val = Int.of_int32 @@ Int32.of_string @@ int_str in
      match int_val with
      | None -> failwith ("Could not parse " ^ int_str ^ " as an int")
      | (Some i) -> LIT_INT i, rest_of_chars rest_str
    else
      failwith ("Syntax error: \"" ^ str_code ^ "\" is invalid")
  | [] -> failwith "Failed to parse token (should not hit this)"


let rec go_lex code tokens =
  match code with
  | [] -> List.rev tokens
  | '{'::rest -> go_lex rest (LBRACE::tokens)
  | '}'::rest -> go_lex rest (RBRACE::tokens)
  | '('::rest -> go_lex rest (LPAREN::tokens)
  | ')'::rest -> go_lex rest (RPAREN::tokens)
  | ';'::rest -> go_lex rest (SEMICOL::tokens)
  | c::more ->
    if Char.is_whitespace c
    then go_lex more tokens
    else
      let tok, rest = lex_complex_token code in
      go_lex rest (tok::tokens)

let lex code =
  let code_list = Base.String.to_list code in
  go_lex code_list []
