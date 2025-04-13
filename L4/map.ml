let rec string_of f = function [] -> "" | h :: t -> f h ^ " " ^ string_of f t

(** [add1 lst] adds 1 to each element of [lst]. *)
let rec add1 = function [] -> [] | h :: t -> (h + 1) :: add1 t

let lst1 = add1 [ 1; 2; 3 ]
let _ = lst1 |> string_of string_of_int |> print_string |> print_newline

(** [concat_bang lst] concatenates "!" to each element of [lst]. *)
let rec concat_bang = function [] -> [] | h :: t -> (h ^ "!") :: concat_bang t

let lst2 = concat_bang [ "sweet"; "salty" ]
let _ = lst2 |> string_of (fun x -> x) |> print_string |> print_newline

(* High-order function *)
let rec map f = function [] -> [] | h :: t -> f h :: map f t
let _add1' = map (fun x -> x + 1)
let _concat_bang' = map (fun x -> x ^ "!")

(* Side effect *)
let p x =
  print_int x;
  print_newline ();
  x + 1

let _lst = map p [ 1; 2 ]

let rec map f = function
  | [] -> []
  | h :: t ->
      let h' = f h in
      h' :: map f t

let _lst = map p [ 1; 2 ]

(* The implementation is awful! O(n2) *)
let map_tr f =
  let rec map_tr_aux f acc = function
    | [] -> acc
    | h :: t -> map_tr_aux f (acc @ [ f h ]) t
  in
  map_tr_aux f []

let rev_map f =
  let rec rev_map_aux acc = function
    | [] -> acc
    | h :: t -> rev_map_aux (f h :: acc) t
  in
  rev_map_aux []

let lst3 = [ 1; 2; 3; 4; 5 ]

let _ =
  lst3
  |> map_tr (fun x -> x * 2)
  |> string_of string_of_int |> print_string |> print_newline

let _ =
  lst3
  |> rev_map (fun x -> x * 2)
  |> string_of string_of_int |> print_string |> print_newline

let map f lst = lst |> rev_map f |> List.rev

let _ =
  lst3
  |> map (fun x -> x * 2)
  |> string_of string_of_int |> print_string |> print_newline
