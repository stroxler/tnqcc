open Core

let show_list (print_item: 'a -> string) (the_list: 'a list) =
  "[\n\t" ^ (String.concat ~sep: ",\n\t" @@ List.map ~f:print_item the_list) ^ "\n]"


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
