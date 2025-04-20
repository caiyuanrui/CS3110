type 'a node = { next : 'a mlist; value : 'a }
and 'a mlist = 'a node option ref

(** [empty()] is an empty singly-linked list. *)
let empty () : 'a mlist = ref None

let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst := Some { next = ref !lst; value = v }

let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with None -> [] | Some { next; value } -> value :: to_list next

let lst0 = empty ()
let lst1 = lst0

let _ =
  insert_first lst0 1;
  insert_first lst0 2;
  insert_first lst0 3

let print_mlist lst =
  List.fold_left
    (fun acc x -> acc ^ " -> " ^ string_of_int x)
    "head" (to_list lst)
  |> print_string |> print_newline

let _ =
  print_mlist lst0;
  print_mlist lst1
