type 'a tree1 = Leaf | Node of 'a * 'a tree1 * 'a tree1

(* the code below constructs this tree:
         4
       /   \
      2     5
     / \   / \
    1   3 6   7
*)
let t1 =
  Node
    ( 4,
      Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)),
      Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) )

let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r
let _ = size t1 |> print_int

type 'a tree2 = Leaf | Node of 'a node2
and 'a node2 = { value : 'a; left : 'a tree2; right : 'a tree2 }

(* represents
      2
     / \
    1   3  *)
let t =
  Node
    {
      value = 2;
      left = Node { value = 1; left = Leaf; right = Leaf };
      right = Node { value = 3; left = Leaf; right = Leaf };
    }

let rec size = function
  | Leaf -> 0
  | Node { value; left; right } -> 1 + size left + size right

let _ = size t |> print_int

let rec mem x = function
  | Leaf -> false
  | Node { value; left; right } -> x = value || mem x left || mem x right

(* O(n2) *)
let rec preorder = function
  | Leaf -> []
  | Node { value; left; right } -> value @ preorder left @ preorder right

(* O(n) *)
let preorder_lin t =
  let rec pre_acc acc = function
    | Leaf -> acc
    | Node { value; left; right } -> value :: pre_acc (pre_acc acc right) left
  in
  pre_acc t
