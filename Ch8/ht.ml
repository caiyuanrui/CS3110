(* Maps as Association Lists *)
module type Map = sig
  type ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val find : 'k -> ('k, 'v) t -> 'v option
  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  val empty : ('k, 'v) t
  val of_list : ('k * 'v) list -> ('k, 'v) t
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module ListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list

  let insert k v m = (k, v) :: m
  let find = List.assoc_opt
  let remove k = List.filter (fun (k', _) -> k' <> k)
  let empty = []
  let of_list lst = lst
  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare
  let binding m k = (k, List.assoc k m)
  let bindings m = List.map (binding m) (keys m)
end

(* Maps as Arrays *)
module type DirectAddressMap = sig
  type 'v t

  val insert : int -> 'v -> 'v t -> unit
  val find : int -> 'v t -> 'v option
  val remove : int -> 'v t -> unit
  val create : int -> 'v t
  val of_list : int -> (int * 'v) list -> 'v t
  val bindings : 'v t -> (int * 'v) list
end

module ArrayMap : DirectAddressMap = struct
  type 'v t = 'v option array

  let insert k v a = a.(k) <- Some v
  let find k a = a.(k)
  let remove k a = a.(k) <- None
  let create n = Array.make n None

  let of_list n lst =
    let a = create n in
    List.iter (fun (k, v) -> insert k v a) lst;
    a

  let bindings a =
    let bs = ref [] in
    let add_binding k v =
      match v with None -> () | Some v -> bs := (k, v) :: !bs
    in
    Array.iteri add_binding a;
    !bs
end

(* Maps as Hash Tables *)
module type TableMap = sig
  type ('k, 'v) t

  val insert : 'k -> 'v -> ('k, 'v) t -> unit
  val find : 'k -> ('k, 'v) t -> 'v option
  val remove : 'k -> ('k, 'v) t -> unit

  val create : ('k -> int) -> int -> ('k, 'v) t
  (** [create hash c] creates a new hashtable with capacity [c]. *)

  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module HashMap : TableMap = struct
  type ('k, 'v) t = {
    hash : 'k -> int;
    mutable size : int;
    mutable buckets : ('k * 'v) list array;
  }

  let capacity { buckets } = Array.length buckets
  let load_factor tab = float_of_int tab.size /. float_of_int (capacity tab)
  let create hash cap = { hash; size = 0; buckets = Array.make cap [] }
  let index k tab = tab.hash k mod capacity tab

  let insert_no_resize k v tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- (k, v) :: List.remove_assoc k old_bucket;
    if not (List.mem_assoc k old_bucket) then tab.size <- tab.size + 1

  let rehash tab new_capacity =
    (* insert [(k, v)] into [tab] *)
    let rehash_binding (k, v) = insert_no_resize k v tab in
    (* insert all bindings of bucket into [tab] *)
    let rehash_bucket bucket = List.iter rehash_binding bucket in
    let old_buckets = tab.buckets in
    tab.buckets <- Array.make new_capacity [];
    tab.size <- 0;
    (* [rehash_binding] is called by [rehash_bucket] once for every binding *)
    Array.iter rehash_bucket old_buckets

  let resize_if_needed tab =
    let lf = load_factor tab in
    if lf > 2.0 then rehash tab (capacity tab * 2)
    else if lf < 0.5 then rehash tab (capacity tab / 2)

  let insert k v tab =
    insert_no_resize k v tab;
    resize_if_needed tab

  let find k tab = List.assoc_opt k tab.buckets.(index k tab)

  let remove_no_resize k tab =
    let b = index k tab in
    let old_bucket = tab.buckets.(b) in
    tab.buckets.(b) <- List.remove_assoc k tab.buckets.(b);
    if List.mem_assoc k old_bucket then tab.size <- tab.size - 1

  let remove k tab =
    remove_no_resize k tab;
    resize_if_needed tab

  let of_list hash lst =
    let m = create hash (2 * List.length lst) in
    List.iter (fun (k, v) -> insert_no_resize k v m) lst;
    m

  let bindings tab =
    Array.fold_left
      (fun acc bucket ->
        List.fold_left (fun acc (k, v) -> (k, v) :: acc) acc bucket)
      [] tab.buckets
end

let t = Hashtbl.create 16

let _ =
  Hashtbl.(
    for i = 1 to 16 do
      add t i (string_of_int i)
    done)

module HashedString : Hashtbl.HashedType = struct
  type t = int

  let equal x y = compare x y = 0
  let hash x = x
end

module T = Hashtbl.Make (HashedString)
