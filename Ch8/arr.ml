module type PersistentArray = sig
  type 'a t
  (** The type of persistent arrays whose elements have type ['a]. The array
      indexing is zero based, meaning that the first element is at index [0],
      and the last element is at index [n - 1], where [n] is the length of the
      array. Any index less than [0] or greater than [n - 1] is out of bounds.
  *)

  val make : int -> 'a -> 'a t
  (** [make n x] is a persistent array of length [n], with each element
      initialized to [x]. Raises [Invalid_argument] if [n] is negative or too
      large for the system. *)

  val length : 'a t -> int
  (** [length a] is the number of elements in [a]. *)

  val get : 'a t -> int -> 'a
  (** [get a i] is the element at index [i] in [a]. Raises [Invalid_argument] if
      [i] is out of bounds. *)

  val set : 'a t -> int -> 'a -> 'a t
  (** [set a i x] is an array that stores [x] at index [i] and otherwise is the
      same as [a]. Raises: [Invalid_argument] if [i] is out of bounds. *)
end

module CopyOnSetArray : PersistentArray = struct
  type 'a t = 'a array

  (** Efficiency: O(n) *)
  let make = Array.make

  (** Efficiency: O(1) *)
  let length = Array.length

  (** Efficiency: O(1) *)
  let get = Array.get

  (** Efficiency: O(n) *)
  let set a i x =
    let a' = Array.copy a in
    a'.(i) <- x;
    a'
end

module VersionTreeArray : PersistentArray = struct
  type 'a t = Base of 'a array | Diff of int * 'a * 'a t

  (** Efficiency: O(n) *)
  let make n x = Base (Array.make n x)

  let rec length = function
    | Base b -> Array.length b
    | Diff (_, _, a) -> length a

  (** Efficiency: O(1) *)
  let set a i x = Diff (i, x, a)

  (** Efficiency: O(k) where k is the depth of the tree *)
  let rec get a i =
    match a with
    | Base b -> b.(i)
    | Diff (j, x, a) -> if j = i then x else get a i
end

module RebasedVersionTreeArray : PersistentArray = struct
  type 'a t = 'a node ref
  and 'a node = Base of 'a array | Diff of int * 'a * 'a t

  (** Efficiency: O(n) *)
  let make n x = ref (Base (Array.make n x))

  (** Efficiency: O(k) where k is the distance to the latest Base node *)
  let rec rebase a =
    match !a with
    | Base b -> b
    | Diff (i, x, a') ->
        let b = rebase a' in
        let old_x = b.(i) in
        b.(i) <- x;
        a := Base b;
        a' := Diff (i, old_x, a);
        b

  (** Efficiency: Same as [rebase]. *)
  let length a = rebase a |> Array.length

  (** Efficiency: Same as [rebase]. *)
  let get a i = (rebase a).(i)

  (** Efficiency: O(1) *)
  let set a i x = ref (Diff (i, x, a))
end
