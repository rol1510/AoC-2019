open Printf

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let print_list l =
  printf "[";
  let rec p = function
    | [] -> ()
    | [a] -> printf "%d" a
    | h :: t -> printf "%d; " h; p t
  in p l;
  printf "]"

let print_list_nl l =
  print_list l;
  printf "\n"

let rec print_list_of_lists = function
  | [] -> ()
  | [a] -> print_list a
  | h :: t -> print_list h; printf "; "; print_list_of_lists t

let print_list_of_lists_nl l =
  print_list_of_lists l;
  printf "\n"
