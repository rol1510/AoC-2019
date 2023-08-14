open Printf
open MyLib

type term =
  | File of int * string
  | Dir of string
  | Cd of string
  | Ls

let show_term = function
  | File (id, path) -> sprintf "File (%d, %s)" id path
  | Dir path -> sprintf "Dir (%s)" path
  | Cd path -> sprintf "Cd (%s)" path
  | Ls -> sprintf "Ls"

type sized = int * string

type 'a tree =
  | Leaf
  | Node of 'a node
and
  'a node = { parent: 'a tree; mutable files: 'a list; mutable dirs: 'a tree list; name: string; mutable size: int }

let parse_command line =
  let parts = Str.split (Str.regexp " ") line in
  let cmd = List.nth parts 1 in
  if cmd = "ls" then
    Ls
  else
    Cd (List.nth parts 2)

let parse_file line =
  let parts = Str.split (Str.regexp " ") line in
  let size = List.hd parts in
  let name = List.nth parts 1 in
  File (int_of_string size, name)

let parse_dir line =
  let start = String.length "dir " in
  let length = (String.length line) - start in
  Dir (String.sub line start length)

let is_ascii_digit c =
  c >= '0' && c <= '9'

(* Assumes the input only uses one "\n" for the new lines no *)
let parse_input input =
  Str.split (Str.regexp "\n") input
  |> List.map String.trim
  |> List.map (fun line ->
      if String.get line 0 = '$' then
        parse_command line
      else if is_ascii_digit (String.get line 0) then
        parse_file line
      else
        parse_dir line
    )

let rec root_of_tree tree =
  match tree with
  | Leaf -> raise (Invalid_argument "root_of_tree: got Leaf")
  | Node n -> match n.parent with
    | Leaf -> tree
    | Node _ -> root_of_tree n.parent

let go_up_one = function
  | Leaf -> raise (Invalid_argument "go_up_one: got Leaf")
  | Node n -> n.parent

let go_into_dir tree dir_name =
  let pred x =
   match x with
    | Node n -> n.name = dir_name
    | Leaf -> false
  in
  match tree with
  | Leaf -> raise (Invalid_argument "go_into_dir: dir not found, got Leaf")
  | Node n ->
    if List.exists pred n.dirs then
      List.find pred n.dirs
    else
      raise (Invalid_argument "go_into_dir: dir not found")

let show_sized s =
  let (size, name) = s in
  sprintf "(%d %s)" size name

let rec show_tree indent tree =
  match tree with
  | Leaf -> printf "Leaf\n"
  | Node n ->
    printf "%s%s %d: " indent n.name n.size;
    show_list show_sized n.files;
    printf "\n";
    List.iter (fun x -> show_tree ("| " ^ indent) x) n.dirs

let add_file tree size name =
  match tree with
  | Leaf -> raise (Invalid_argument "add_file: got Leaf")
  | Node n ->
    let lst = List.append n.files [(size, name)] in
    n.files <- lst;
    (* n.size <- n.size + size; *)
    tree

let add_dir tree dir =
  match tree with
  | Leaf -> raise (Invalid_argument "add_dir: got Leaf")
  | Node n ->
    let new_node = Node { parent = tree; files = []; dirs = []; name = dir; size = 0 } in
    if List.exists (function
      | Leaf -> raise (Invalid_argument "add_dir: got Leaf")
      | Node n -> n.name = dir) n.dirs then
      raise (Invalid_argument "add_dir: dir already exists")
    else
      let lst = List.append n.dirs [new_node] in
      n.dirs <- lst;
      tree

let build_tree commands =
  let rec aux root tree command =
    match command with
    | Cd path -> (match path with
      | "/" -> root
      | ".." -> go_up_one tree
      | dir -> go_into_dir tree dir)
    | Ls -> tree (* Todo: why do we parse the ls's???? *)
    | Dir name -> add_dir tree name
    | File (size, name) -> add_file tree size name
  in
    let root = Node { parent = Leaf; files = []; dirs = []; name = "/"; size = 0 } in
    let _ = List.fold_left (aux root) root commands in
    root

let update_dir_sizes tree =
  let rec aux tree =
    match tree with
    | Leaf -> 0
    | Node n ->
      let dirs = List.fold_left (fun acc x -> acc + aux x) 0 n.dirs in
      let files = List.fold_left (fun acc x -> acc + (fst x)) 0 n.files in
      n.size <- dirs + files;
      n.size
  in
    let _ = aux tree in ()

let sum_dirs_at_most x tree =
  let rec aux acc tree =
    match tree with
    | Leaf -> acc
    | Node n ->
      let s = List.fold_left (fun acc x -> aux acc x) acc n.dirs in
      if n.size <= x then
        s + n.size
      else
        s
  in
    aux 0 tree

let part_1 msg input =
  let commands = parse_input input in
  (* show_list_tall show_term commands; *)
  let tree = build_tree commands in
  update_dir_sizes tree;
  (* show_tree "" tree; *)
  let sum = sum_dirs_at_most 100_000 tree in
  printf "Result %s: %d\n" msg sum


let find_smallest_to_delete minimum tree =
  let rec aux = function
    | Leaf -> raise (Invalid_argument "find_smallest_to_delete: got Leaf")
    | Node n -> List.fold_left (fun acc x ->
      let s = (aux x) in
      if s >= minimum then
        if s < acc then
          s
        else
          acc
      else
        acc
    ) n.size n.dirs
  in
    aux tree

let get_dir_size = function
  | Leaf -> raise (Invalid_argument "get_dir_size: got Leaf")
  | Node n -> n.size

let part_2 msg input =
  let commands = parse_input input in
  (* show_list_tall show_term commands; *)
  let tree = build_tree commands in
  update_dir_sizes tree;
  (* show_tree "" tree; *)
  let to_free = (get_dir_size tree) - 40_000_000 in
  (* printf "To free: %d\n" to_free; *)
  let sum = find_smallest_to_delete to_free tree in
  printf "Result %s: %d\n" msg sum


let () = part_1 "test 1" (read_whole_file "./day_7.test")
let () = part_1 "part 1" (read_whole_file "./day_7.input")
let () = part_2 "test 2" (read_whole_file "./day_7.test")
let () = part_2 "part 2" (read_whole_file "./day_7.input")
