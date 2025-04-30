module type Set = sig
  type 'a t

  val empty : 'a t
  val mem : 'a -> 'a t -> bool
  val insert : 'a -> 'a t -> 'a t
end

module BstSet : Set = struct
  type 'a t = Leaf | Node of 'a * 'a t * 'a t

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (y, l, r) -> if y = x then true else mem x (if x < y then l else r)

  let rec insert x = function
    | Leaf -> Node (x, Leaf, Leaf)
    | Node (y, l, r) as t ->
        if x < y then Node (y, insert x l, r)
        else if x > y then Node (y, l, insert x r)
        else t
end
