open Printf
open MyLib
open Parcoom

type move = {
  from: int;
  to_: int;
  count: int;
}

let make_move from to_ count = {
  from = (int_of_string from) - 1;
  to_ = (int_of_string to_) - 1;
  count = int_of_string count;
}

let space =
  prefix " "

let number =
  parse_while (fun c -> c >= '0' && c <= '9')

let bottom_parser =
  (prefix "move " *> number <*>
   prefix " from " *> number <*>
   prefix " to " *> number <* optional (prefix "\n"))

let handle_result m =
  List.map (fun ((count, from), to_) -> make_move from to_ count) m

let parse_bottom bottom =
  let result = bottom
    |> Parcoom.run (many bottom_parser)
  in
    match result with
    | Ok r -> handle_result r
    | Error e -> printf "Error: %s at %d\n" e.desc e.pos; []

let parse_crate_count s =
  Str.split (Str.regexp "-") s
  |> List.filter (fun s -> String.length s > 0)
  |> List.rev
  |> List.hd
  |> int_of_string

let parse_crates s =
  List.map (fun s -> window_4 (explode ((s) ^ "-"))) s
  |> List.map (List.map (fun w -> List.nth w 1))
  |> pivot_lists
  |> List.map (List.filter (fun c -> c <> '-'))

let parse_top top =
  let lines = Str.split (Str.regexp "\n") top in
  parse_crates (everything_but_last lines)

(* Assumes the input only uses one "\n" for the new lines no *)
(* I changed the spaces in the crates definition to dashes ('-'), so we can
   relay on its formatting for parsing. Makes it a little easier. *)
let parse_input input =
  let parts = Str.split (Str.regexp "\n\n") input in
  let top = List.hd parts in
  let t = parse_top top in
  let bottom = List.hd (List.tl parts) in
  let b = parse_bottom bottom in
  (t, b)

let apply_move append_func crates (m: move) =
  let f = List.nth crates m.from in
  let t = List.nth crates m.to_ in
  let parts = MyLib.split m.count f in
  let f' = snd parts in
  let t' = append_func (fst parts) t in
  let crates' = replace_at crates m.from f' in
  let crates'' = replace_at crates' m.to_ t' in
  crates''

let get_top_crates crates =
  List.map (fun c -> List.hd c) crates

let part_1 msg input =
  let x = parse_input input in
  let crates = fst x in
  let moves = snd x in
  List.fold_left (
    fun crates m -> (apply_move List.rev_append) crates m
  ) crates moves
  |> get_top_crates
  |> string_of_char_list
  |> printf "Result %s: %s\n" msg


let part_2 msg input =
  let x = parse_input input in
  let crates = fst x in
  let moves = snd x in
  List.fold_left (
    fun crates m -> (apply_move List.append) crates m
  ) crates moves
  |> get_top_crates
  |> string_of_char_list
  |> printf "Result %s: %s\n" msg


let () = part_1 "test 1" (read_whole_file "./day_5.test")
let () = part_1 "part 1" (read_whole_file "./day_5.input")
let () = part_2 "test 2" (read_whole_file "./day_5.test")
let () = part_2 "part 2" (read_whole_file "./day_5.input")
