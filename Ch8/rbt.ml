(** 1. BST 2. Local invariant: No red node has a red child 3. Global invariant:
    Every path from root to a leaf node has the same number of black nodes *)
module RBTree = struct
  type color = Red | Black
  type 'a t = Leaf | Node of color * 'a * 'a t * 'a t

  let empty = Leaf

  (** Efficiency: O(log n) *)
  let rec mem x = function
    | Leaf -> false
    | Node (_, y, l, r) ->
        if x < y then mem x l else if x > y then mem x r else true

  let balance = function
    | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
    | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
    | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
    | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
    | a, b, c, d -> Node (a, b, c, d)

  (** Efficiency: O(log n) *)
  let insert x s =
    let rec insert_aux x = function
      | Leaf -> Node (Red, x, Leaf, Leaf)
      | Node (c, v, l, r) as n ->
          if x < v then balance (c, v, insert_aux x l, r)
          else if x > v then balance (c, v, r, insert_aux x r)
          else n
    in
    match insert_aux x s with
    | Leaf -> failwith "unreachable"
    | Node (_, v, l, r) -> Node (Black, v, l, r)
end
