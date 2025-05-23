module type Map = sig
  type ('k, 'v) t
  (** [('k, 'v) t] is the type of maps that bind keys of type ['k] to values of
      type ['v]. *)

  val empty : ('k, 'v) t
  (** [empty] does not bind any keys. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [insert k v m] is the map that binds [k] to [v], and also contains all the
      bindings of [m]. If [k] was already bound in [m], that old binding is
      superseded by the binding to [v] in the returned map. *)

  val lookup : 'k -> ('k, 'v) t -> 'v
  (** [lookup k m] is the value bound to [k] in [m]. Raises: [Not_found] if [k]
      is not bound in [m]. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is an association list containing the same bindings as [m].
      The keys in the list are guaranteed to be unique. *)
end

module AssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list

  let empty = []
  let insert k v m = (k, v) :: m
  let lookup k m = List.assoc k m
  let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)
  let bindings m = m |> keys |> List.map (fun k -> (k, lookup k m))
end

let print_list lst =
  let str =
    List.fold_left
      (fun acc (x, y) -> acc ^ x ^ "->" ^ string_of_float y ^ " ")
      "" lst
  in
  print_endline str

let _ =
  let open AssocListMap in
  let m = empty |> insert "pi" 3.14 |> insert "e" 2.718 in
  let m' = m |> insert "phi" 1.618 in
  let b = bindings m in
  let b' = bindings m' in
  (b |> print_list, b' |> print_list)
