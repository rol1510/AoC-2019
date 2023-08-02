open Printf

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let print_list fmt l =
  printf "[";
  let rec p = function
    | [] -> ()
    | [a] -> printf fmt a
    | h :: t -> printf fmt h; printf "; "; p t
  in p l;
  printf "]"

let print_list_nl fmt l =
  print_list fmt l;
  printf "\n"

let rec print_list_of_lists fmt = function
  | [] -> ()
  | [a] -> print_list fmt a
  | h :: t -> print_list fmt h; printf "; "; print_list_of_lists fmt t

let print_list_of_lists_nl fmt l =
  print_list_of_lists fmt l;
  printf "\n"
