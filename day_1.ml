open Str
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


(* Assumes the input only uses one "\n" for the new lines no *)
let parse_input input =
  Str.split (Str.regexp "\n\n") input
  |> List.map (Str.split (Str.regexp "\n"))
  |> List.map (List.map int_of_string)

let sum_of k l =
  let rec f k sum = function
    | [] -> 0
    | h :: t -> if k == 1 then h + sum else f (k - 1) (h + sum) t
  in f k 0 l

let part_1 msg input =
  parse_input input
  |> List.map (List.fold_left (+) 0)
  |> List.fold_left (max) (-1)
  |> printf "Result %s: %d\n" msg


let part_2 msg input =
  parse_input input
  |> List.map (List.fold_left (+) 0)
  |> List.sort (compare)
  |> List.rev
  |> sum_of 3
  |> printf "Result %s: %d\n" msg

let () = part_1 "test 1" (read_whole_file "./day_1.test")
let () = part_1 "part 1" (read_whole_file "./day_1.input")
let () = part_2 "test 2" (read_whole_file "./day_1.test")
let () = part_2 "part 2" (read_whole_file "./day_1.input")
