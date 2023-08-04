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

let rec window_3 = function
  | [] -> []
  | a :: b :: c :: t -> [a; b; c] :: window_3 (t)
  | t -> raise (Invalid_argument "list ist node divisible by 3")

let rec window_4 = function
  | [] -> []
  | a :: b :: c :: d :: t -> [a; b; c; d] :: window_4 (t)
  | t -> raise (Invalid_argument "list ist node divisible by 4")

let rec pivot_lists lists =
  match lists with
  | [] -> []
  | [] :: _ -> []
  | _ -> List.map List.hd lists :: pivot_lists (List.map List.tl lists)

let rec take n = function
  | [] -> []
  | h :: t -> if n = 0 then [] else h :: take (n - 1) t

let rec split n l =
  if n = 0 then ([], l)
  else
    match l with
    | [] -> ([], [])
    | h :: t ->
        let (l1, l2) = split (n - 1) t in
        (h :: l1, l2)

let replace_at l n a =
  let (l1, l2) = split n l in
  let l2' = List.tl l2 in
  l1 @ [a] @ l2'

let last l =
  List.hd (List.rev l)

let everything_but_last l =
  List.rev (List.tl (List.rev l))

let explode s =
  List.init (String.length s) (String.get s)

let string_of_char_array arr =
  String.init (Array.length arr) (fun i -> arr.(i))

let string_of_char_list lst: string =
  let buf = Buffer.create (List.length lst) in
  List.iter (Buffer.add_char buf) lst;
  Buffer.contents buf
