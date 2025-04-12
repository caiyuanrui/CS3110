(* Let's transform this factorial to be tail recursive *)
(* let rec fact_aux n acc = if n = 0 then acc else fact_aux (n - 1) (acc * n)
let fact_tr n = fact_aux n 1

let () = print_int (fact_tr 10) *)

let _ = print_endline "Hello" in
let _ = print_endline "The" in
print_endline "World"
;;

(* Syntax Sugar *)
print_endline "Hello";
print_endline "The";
print_endline "World"

(* Ignore a potentially useful value.
It's easy to implement it yourself: `let ignore _ = ()` *)
(* ignore 1;; *)

(* let print_stat name num =
  Printf.printf "%s : %F\n%!" name num
let string_of_stat name num=
  Printf.sprintf "%s : %F" name num *)
