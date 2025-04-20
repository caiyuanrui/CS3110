(* Exercise: complex synonym [★]

Here is a module type for complex numbers, which have a real and imaginary component:

module type ComplexSig = sig
  val zero : float * float
  val add : float * float -> float * float -> float * float
end
Improve that code by adding type t = float * float. Show how the signature can be written more tersely because of the type synonym. *)

module type ComplexSig = sig
  type t = float * float

  val zero : t
  val add : t -> t -> t
end

(* Exercise: binary search tree map [★★★★]

Write a module BstMap that implements the Map module type using a binary search tree type. Binary trees were covered earlier when we discussed algebraic data types. A binary search tree (BST) is a binary tree that obeys the following BST Invariant:

For any node n, every node in the left subtree of n has a value less than n’s value, and every node in the right subtree of n has a value greater than n’s value.

Your nodes should store pairs of keys and values. The keys should be ordered by the BST Invariant. Based on that invariant, you will always know whether to look left or right in a tree to find a particular key. *)
module BstMap (Key : Map.OrderedType) = struct
  type key = Key.t
  type 'a t = Empty | Node of { l : 'a t; v : key; d : 'a; r : 'a t }

  let empty = Empty

  let rec add key value = function
    | Empty -> Node { l = Empty; v = key; d = value; r = Empty }
    | Node { l; v; d; r } ->
        let c = Key.compare key v in
        if c = 0 then Node { l; v; d = value; r }
        else if c < 0 then Node { l = add key value l; v; d; r }
        else Node { l; v; d; r = add key value r }

  let rec find x = function
    | Empty -> raise Not_found
    | Node { l; v; d; r } ->
        let c = Key.compare x v in
        if c = 0 then d else find x (if c < 0 then l else r)
end

module IntBstMap = BstMap (Int)
