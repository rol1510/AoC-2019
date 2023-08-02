open Printf
open MyLib

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
