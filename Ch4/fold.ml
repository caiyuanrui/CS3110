let rec sum = function [] -> 0 | h :: t -> h + sum t
let s = sum [ 1; 2; 3; 4; 5 ]
let rec concat = function [] -> "" | h :: t -> h ^ concat t
let c = concat [ "foo"; "bar"; "baz" ]

let _ =
  print_int s;
  print_newline ();
  print_string c;
  print_newline ()

let rec combine f acc = function [] -> acc | h :: t -> f h (combine f acc t)
let sum' = combine ( + ) 0
let concat' = combine ( ^ ) ""

let _ =
  print_int (sum' [ 1; 2; 3; 4; 5 ]);
  print_newline ();
  print_string (concat' [ "foo"; "bar"; "baz" ]);
  print_newline ()

let rec fold_right f lst (acc : 'acc) =
  match lst with [] -> acc | h :: t -> f h (fold_right f t acc)

let _ =
  print_string (fold_right ( ^ ) [ "foo"; "bar"; "baz" ] "");
  print_newline ()

let rec combine_tr f acc = function
  | [] -> acc
  | h :: t -> combine_tr f (f acc h) t

let _ =
  print_int (combine_tr ( + ) 0 [ 1; 2; 3; 4; 5 ]);
  print_newline ();
  print_string (combine_tr ( ^ ) "" [ "foo"; "bar"; "baz" ]);
  print_newline ();
  print_int (combine ( - ) 0 [ 3; 2; 1 ]);
  print_newline ();
  print_int (combine_tr ( - ) 0 [ 3; 2; 1 ]);
  print_newline ()

let rec _fold_left f acc = function
  | [] -> acc
  | h :: t -> _fold_left f (f acc h) t

let length = List.fold_left (fun acc _ -> acc + 1) 0

let _ =
  print_string "the length of [1;2;3] is ";
  print_int (length [ 1; 2; 3 ]);
  print_newline ()

let rev = List.fold_left (fun acc x -> x :: acc) []

let _ =
  [ 1; 2; 3; 4 ] |> rev
  |> List.map (fun x -> string_of_int x ^ " ")
  |> List.fold_left (fun acc x -> acc ^ x) ""
  |> print_string |> print_newline

let map f lst = List.fold_right (fun x acc -> f x :: acc) lst []

let _ =
  [ 1; 2; 3; 4 ]
  |> map (fun x -> x * 2)
  |> List.map (fun x -> string_of_int x ^ " ")
  |> List.fold_left (fun acc x -> acc ^ x) ""
  |> print_string |> print_newline
