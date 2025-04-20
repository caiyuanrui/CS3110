module type OrderedType = sig
  type t

  val compare : t -> t -> int
  (** The comparison should return 0 if two keys are equal.

      The comparison should return a strictly negative number if the first key
      is lesser than the second.

      The comparison should return a strictly positive number if the first key
      is greater than the second. *)
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val mem : key -> 'a t -> bool
  val find : key -> 'a t -> 'a

  (* ... *)
end

module Make (Ord : OrderedType) = struct
  type key = Ord.t

  type 'a t =
    | Empty
    | Node of { l : 'a t; v : key; d : 'a; r : 'a t; h : int }
        (** Left subtree, key, value/data, right subtree, height of node. *)

  let empty = Empty

  let rec mem x = function
    | Empty -> false
    | Node { l; v; r } ->
        let c = Ord.compare x v in
        c = 0 || mem x (if c < 0 then l else r)

  let rec find x = function
    | Empty -> raise Not_found
    | Node { l; v; r; d } ->
        let c = Ord.compare x v in
        if c = 0 then d else find x (if c < 0 then l else r)
end

type name = { first : string; last : string }

module Name = struct
  type t = name

  let compare { first = f1; last = l1 } { first = f2; last = l2 } =
    match String.compare l1 l2 with 0 -> String.compare f1 f2 | c -> c
end

module _ = Make (Name)
