module type Set = sig
  type 'a t
  (** ['a t] is the type of sets whose elements are of type ['a]. *)

  val empty : 'a t
  (** [empty] is the empty set. *)

  val mem : 'a -> 'a t -> bool
  (** [mem x s] is whether [x] is an element of [s]. *)

  val add : 'a -> 'a t -> 'a t
  (** [add x s] is the set that contains [x] and all the elements of [s]. *)

  val elements : 'a t -> 'a list
  (** [elements s] is a list containing the elements of [s]. No guarantee is
      made about the ordering of that list, but each element is guaranteed to be
      unique. *)
end

module UniqListSet : Set = struct
  type 'a t = 'a list

  let empty = []
  let mem = List.mem
  let add x s = if mem x s then s else x :: s
  let elements = Fun.id
end
