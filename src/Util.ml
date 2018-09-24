open Base

let show_list (print_item: 'a -> string) (the_list: 'a list) =
  "[" ^ (String.concat ~sep: ", " @@ List.map ~f:print_item the_list) ^ "]"

