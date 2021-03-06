open Core

let show_list (print_item: 'a -> string) (the_list: 'a list) =
  "[" ^ (String.concat ~sep: "; " @@ List.map ~f:print_item the_list) ^ "]"


let map_lines (inx: In_channel.t) ~(f: string -> 'a) =
  let out : 'a list ref = ref [] in
  let rec go () =
    let line = In_channel.input_line ~fix_win_eol: true inx in
    match line with
    | Some text ->
      out := (f text) :: !out;
      go ()
    | None -> ();
  in go ();
  List.rev !out


let map_line_nos inx ~(f : int -> string -> 'a) =
  let i: int ref = ref 0 in
  map_lines inx ~f:(
    fun line ->
      let out = f (!i) line in
      i := !i + 1;
      out
  )

type os_type =
    OSX | Linux
    [@@deriving eq]

let current_os =
  let exitcode = Sys.command
      "bash -c 'if [[ $(uname -a) =~ Darwin.* ]]; then exit 1; fi'" in
  if exitcode = 1
  then OSX
  else Linux
