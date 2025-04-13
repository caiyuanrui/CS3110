type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* Function to print a tree *)
let rec print_tree = function
  | Leaf -> print_string "Leaf "
  | Node (value, left, right) ->
      print_string "Node (";
      print_int value;
      (* Adjust this to the type of your tree's data *)
      print_string ", ";
      print_tree left;
      print_string ", ";
      print_tree right;
      print_string ")"

let tr =
  Node
    ( 1,
      Node (2, Node (5, Leaf, Leaf), Leaf),
      Node (3, Leaf, Node (4, Leaf, Leaf)) )

(* Map on Trees *)
let rec map f = function
  | Leaf -> Leaf
  | Node (v, l, r) -> Node (f v, map f l, map f r)

let _ = tr |> map (( + ) 1) |> print_tree |> print_newline

(* Fold on Trees *)
let rec fold f acc = function
  | Leaf -> acc
  | Node (v, l, r) -> f v (fold f acc l) (fold f acc r)

let _ = tr |> fold (fun x y z -> x + y + z) 0 |> print_int |> print_newline

(* Filter on Trees *)
let rec filter p = function
  | Leaf -> Leaf
  | Node (v, l, r) -> if p v then Node (v, filter p l, filter p r) else Leaf
