open Printf
open MyLib

module CSet = Set.Make(Char)

let parse_input input =
  String.trim input

let check_all_unique s =
  let set = CSet.of_seq (String.to_seq s) in
  CSet.cardinal set = String.length s

let check n s =
  let rec check_next s current =
    let start = String.sub s 0 n in
    if check_all_unique start then
      current + n
    else
      check_next (String.sub s 1 (String.length s - 1)) (current + 1)
  in
    check_next s 0


let part_1 msg input =
  parse_input input
  |> check 4
  |> printf "Result %s: %d\n" msg

let part_2 msg input =
  parse_input input
  |> check 14
  |> printf "Result %s: %d\n" msg


let () = part_1 "test 1-1" (read_whole_file "./day_6.test1")
let () = part_1 "test 1-2" (read_whole_file "./day_6.test2")
let () = part_1 "test 1-3" (read_whole_file "./day_6.test3")
let () = part_1 "test 1-4" (read_whole_file "./day_6.test4")
let () = part_1 "test 1-5" (read_whole_file "./day_6.test5")
let () = part_1 "part 1" (read_whole_file "./day_6.input")
let () = part_2 "test 2-1" (read_whole_file "./day_6.test1")
let () = part_2 "test 2-2" (read_whole_file "./day_6.test2")
let () = part_2 "test 2-3" (read_whole_file "./day_6.test3")
let () = part_2 "test 2-4" (read_whole_file "./day_6.test4")
let () = part_2 "test 2-5" (read_whole_file "./day_6.test5")
let () = part_2 "part 2" (read_whole_file "./day_6.input")
