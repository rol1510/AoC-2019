open Printf
open MyLib

module CSet = Set.Make(Char)

let set_from_string str =
  String.to_seq str
  |> CSet.of_seq

let split_string_in_half s =
  let half = String.length s / 2 in
  let first = String.sub s 0 half in
  let second = String.sub s half (String.length s - half) in
  [first; second]

(* Assumes the input only uses one "\n" for the new lines no *)
let parse_input input =
  Str.split (Str.regexp "\n") input

let map_char_to_priority c =
  match c with
  | 'a' .. 'z' -> Char.code c - Char.code 'a' + 1
  | 'A' .. 'Z' -> Char.code c - Char.code 'A' + 27
  | a -> failwith (sprintf "Invalid character: '%c'" a)

let part_1 msg input =
  parse_input input
  |> List.map split_string_in_half
  |> List.map (List.map set_from_string)
  |> List.map (function
      | [first; second] -> CSet.inter first second
      | _ -> failwith "here we only expect 2 elements per list")
  |> List.map CSet.choose (* sets only have 1 element *)
  |> List.map map_char_to_priority
  |> List.fold_left (+) 0
  |> printf "Result %s: %d\n" msg

let find_common_char l =
  let rec inner base = function
    | [] -> base
    | h :: t -> inner (CSet.inter base (set_from_string h)) t
  in
    inner (set_from_string (List.hd l)) (List.tl l)
    |> CSet.choose (* should only have one result *)

let part_2 msg input =
  parse_input input
  |> window_3
  |> List.map find_common_char
  |> List.map map_char_to_priority
  |> List.fold_left (+) 0
  |> printf "Result %s: %d\n" msg

let () = part_1 "test 1" (read_whole_file "./day_3.test")
let () = part_1 "part 1" (read_whole_file "./day_3.input")
let () = part_2 "test 2" (read_whole_file "./day_3.test")
let () = part_2 "part 2" (read_whole_file "./day_3.input")

