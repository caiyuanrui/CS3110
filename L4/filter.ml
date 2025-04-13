let rec string_of f = function [] -> "" | h :: t -> f h ^ " " ^ string_of f t
let even x = x mod 2 = 0
let odd x = x mod 2 <> 0
let lst = [ 1; 2; 3; 4; 5; 6; 7 ]

let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t

let evens = lst |> filter even
let _ = evens |> string_of string_of_int |> print_string |> print_newline
let odds = lst |> filter odd
let _ = odds |> string_of string_of_int |> print_string |> print_newline

let rev_filter p =
  let rec rev_filter_aux acc = function
    | [] -> acc
    | h :: t ->
        if p h then rev_filter_aux (h :: acc) t else rev_filter_aux acc t
  in
  rev_filter_aux []

let _ =
  lst |> rev_filter even |> string_of string_of_int |> print_string
  |> print_newline

let _ =
  lst |> rev_filter odd |> string_of string_of_int |> print_string
  |> print_newline

let filter p lst = lst |> rev_filter p |> List.rev

let _ =
  lst |> filter even |> string_of string_of_int |> print_string |> print_newline

let _ =
  lst |> filter odd |> string_of string_of_int |> print_string |> print_newline
