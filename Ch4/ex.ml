open OUnit2

let string_of_list f = List.fold_left (fun acc x -> acc ^ f x) ""
let id x = x

(* Exercise: account balance [★★★]

Write a function which, given a list of numbers representing debits, deducts them from an account balance, and finally returns the remaining amount in the balance. Write three versions: fold_left, fold_right, and a direct recursive implementation. *)
let fold_left acc = List.fold_left (fun acc x -> acc - x) acc
let fold_right acc lst = List.fold_right (fun x acc -> acc - x) lst acc

let tests f p =
  [
    ( p ^ "normal transaction" >:: fun _ ->
      assert_equal 4 (f 10 [ 1; 2; 3 ]) ~printer:string_of_int );
    ( p ^ "no debits" >:: fun _ ->
      assert_equal 10 (f 10 []) ~printer:string_of_int );
    ( p ^ "deduct too much" >:: fun _ ->
      assert_equal (-5) (f 10 [ 5; 10 ]) ~printer:string_of_int );
  ]

let _ =
  "Exercise: account balance [★★★]"
  >::: tests fold_left "fold_left" @ tests fold_right "fold_right"
  |> run_test_tt_main

(* Exercise: map composition [★★★]

Show how to replace any expression of the form [List.map f (List.map g lst)] with an equivalent expression that calls List.map only once. *)
let mmap f g lst = List.map (fun x -> f (g x)) lst
let expected_mmap f g lst = List.map f (List.map g lst)

let assert_eq f g lst =
 fun _ ->
  assert_equal (expected_mmap f g lst) (mmap f g lst)
    ~printer:(string_of_list string_of_int)

let tests f p =
  [
    (* Test normal case where f and g are simple transformations *)
    p ^ "normal case" >:: assert_eq (( + ) 1) (( * ) 2) [ 2; 3; 4 ];
    (* Test with identity functions: should return the same list *)
    p ^ "identity functions" >:: assert_eq id id [ 2; 3; 4 ];
    (* Test with an empty list: should return an empty list *)
    p ^ "empty list" >:: assert_eq (( + ) 1) (( * ) 2) [];
    (* Test with no transformation: the list should remain unchanged *)
    p ^ "no transformation" >:: assert_eq id id [ 2; 3; 4 ];
  ]

let _ =
  "Exercise: map composition [★★★]" >::: tests mmap "mmap" |> run_test_tt_main

(* Exercise: more list fun [★★★]

Write functions that perform the following computations. Each function that you write should use one of List.fold, List.map or List.filter. To choose which of those to use, think about what the computation is doing: combining, transforming, or filtering elements.

Find those elements of a list of strings whose length is strictly greater than 3.

Add 1.0 to every element of a list of floats.

Given a list of strings strs and another string sep, produce the string that contains every element of strs separated by sep. For example, given inputs ["hi";"bye"] and ",", produce "hi,bye", being sure not to produce an extra comma either at the beginning or end of the result string. *)
let greater_than_3 = List.filter (fun x -> x > 3)
let add_one = List.map (( +. ) 1.0)

let concat strs sep =
  match strs with
  | [] -> ""
  | h :: t -> h ^ List.fold_right (fun s acc -> sep ^ s ^ acc) t ""

let test_greater_than_3 =
  [
    ( "greater_than_3 normal case" >:: fun _ ->
      assert_equal [ 6; 4; 9; 7 ]
        (greater_than_3 [ 6; 2; 3; 4; 1; 9; 7 ])
        ~printer:(string_of_list string_of_int) );
    ( "greater_than_3 empty list" >:: fun _ ->
      assert_equal [] (greater_than_3 [])
        ~printer:(string_of_list string_of_int) );
    ( "greater_than_3 all greater than 3" >:: fun _ ->
      assert_equal [ 6; 4; 9; 7 ]
        (greater_than_3 [ 6; 4; 9; 7 ])
        ~printer:(string_of_list string_of_int) );
    ( "greater_than_3 all not greater than 3" >:: fun _ ->
      assert_equal []
        (greater_than_3 [ 1; 3; 2; 1; 0; 3; 2; 1 ])
        ~printer:(string_of_list string_of_int) );
  ]

let rec eq lst1 lst2 =
  match lst1 with
  | [] -> lst2 = []
  | h1 :: t1 -> (
      match lst2 with
      | [] -> false
      | h2 :: t2 -> (h1 -. h2 < 1e-6 || h2 -. h1 < 1e-6) && eq t1 t2)

let test_add_one =
  [
    ( "add_one normal case" >:: fun _ ->
      assert_equal
        [ 1.618; 2.414; 3.718; 4.142 ]
        (add_one [ 0.618; 1.414; 2.718; 3.142 ])
        ~printer:(string_of_list string_of_float)
        ~cmp:eq );
  ]

let test_concat =
  [
    ( "concat normal case" >:: fun _ ->
      assert_equal "hi,bye" (concat [ "hi"; "bye" ] ",") );
    ("concat empty list" >:: fun _ -> assert_equal "" (concat [] ","));
    ( "concat single-element list" >:: fun _ ->
      assert_equal "hi" (concat [ "hi" ] ",") );
  ]

let _ =
  "Exercise: more list fun [★★★]"
  >::: test_greater_than_3 @ test_add_one @ test_concat
  |> run_test_tt_main

(* Exercise: association list keys [★★★]

Recall that an association list is an implementation of a dictionary in terms of a list of pairs, in which we treat the first component of each pair as a key and the second component as a value.

Write a function keys: ('a * 'b) list -> 'a list that returns a list of the unique keys in an association list. Since they must be unique, no value should appear more than once in the output list. The order of values output does not matter. How compact and efficient can you make your solution? Can you do it in one line and linearithmic space and time? Hint: List.sort_uniq. *)

let rec keys = function [] -> [] | (h1, _) :: t -> h1 :: keys t

(** reversed version *)
let keys =
  let rec keys_aux acc = function
    | [] -> acc
    | (h1, _) :: t -> keys_aux (h1 :: acc) t
  in
  keys_aux []

(* Exercise: valid matrix [★★★]

A mathematical matrix can be represented with lists. In row-major representation, this matrix

1 1 1
9 8 7

would be represented as the list [[1; 1; 1]; [9; 8; 7]]. Let’s represent a row vector as an int list. For example, [9; 8; 7] is a row vector.

A valid matrix is an int list list that has at least one row, at least one column, and in which every column has the same number of rows. There are many values of type int list list that are invalid, for example,

[]

[[1; 2]; [3]]

Implement a function is_valid_matrix: int list list -> bool that returns whether the input matrix is valid. Unit test the function. *)
let is_valid_matrix =
  let rec is_valid_matrix_aux cols = function
    | [] -> true
    | h :: t -> (
        match cols with
        | None -> is_valid_matrix_aux (Some (List.length h)) t
        | Some c ->
            if c <> List.length h then false else is_valid_matrix_aux (Some c) t
        )
  in
  is_valid_matrix_aux None

let test_is_valid_matrix =
  [
    ( "empty matrix" >:: fun _ ->
      assert_equal true (is_valid_matrix []) ~printer:string_of_bool );
    ( "scalar matrix" >:: fun _ ->
      assert_equal true (is_valid_matrix [ [ 1 ] ]) ~printer:string_of_bool );
    ( "not a matrix" >:: fun _ ->
      assert_equal false
        (is_valid_matrix [ [ 1; 2 ]; [ 3 ] ])
        ~printer:string_of_bool );
    ( "square matrix" >:: fun _ ->
      assert_equal true
        (is_valid_matrix [ [ 1; 2 ]; [ 3; 4 ] ])
        ~printer:string_of_bool );
    ( "non square matrix" >:: fun _ ->
      assert_equal true
        (is_valid_matrix [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ])
        ~printer:string_of_bool );
  ]

let _ =
  "Exercise: valid matrix [★★★]" >::: test_is_valid_matrix |> run_test_tt_main

(* Exercise: row vector add [★★★]

Implement a function add_row_vectors: int list -> int list -> int list for the element-wise addition of two row vectors. For example, the addition of [1; 1; 1] and [9; 8; 7] is [10; 9; 8]. If the two vectors do not have the same number of entries, the behavior of your function is unspecified—that is, it may do whatever you like. Hint: there is an elegant one-line solution using List.map2. Unit test the function. *)
let add_row_vectors = List.map2 ( + )

let test_add_row_vectors =
  [
    ( "normal case" >:: fun _ ->
      assert_equal [ 10; 9; 8 ] (add_row_vectors [ 1; 1; 1 ] [ 9; 8; 7 ]) );
    ( "shapes mismatch" >:: fun _ ->
      assert_raises (Invalid_argument "List.map2") (fun _ ->
          add_row_vectors [ 1; 1 ] [ 9; 8; 7 ]) );
  ]

let _ =
  "Exercise: row vector add [★★★]" >::: test_add_row_vectors |> run_test_tt_main

(* Exercise: matrix add [★★★]

Implement a function add_matrices: int list list -> int list list -> int list list for matrix addition. If the two input matrices are not the same size, the behavior is unspecified. Hint: there is an elegant one-line solution using List.map2 and add_row_vectors. Unit test the function. *)

let add_matrices = List.map2 add_row_vectors

let test_add_matrices =
  [
    ( "normal case" >:: fun _ ->
      assert_equal
        [ [ 10; 9; 8 ]; [ 12; 11; 10 ] ]
        (add_matrices [ [ 1; 1; 1 ]; [ 4; 3; 2 ] ] [ [ 9; 8; 7 ]; [ 8; 8; 8 ] ])
    );
    ( "shapes mismatch" >:: fun _ ->
      assert_raises (Invalid_argument "List.map2") (fun _ ->
          add_matrices [ [ 1; 1 ] ] [ [ 9; 8; 7 ] ]) );
  ]

let _ = "Exercise: matrix add [★★★]" >::: test_add_matrices |> run_test_tt_main

(* Exercise: matrix multiply [★★★★]

Implement a function multiply_matrices: int list list -> int list list -> int list list for matrix multiplication. If the two input matrices are not of sizes that can be multiplied together, the behavior is unspecified. Unit test the function. Hint: define functions for matrix transposition and row vector dot product. *)
let multiply_row_vectors = List.map2 ( * )
let multiply_matrices = List.map2 multiply_row_vectors

let test_multiply_matrices =
  [
    ( "normal case" >:: fun _ ->
      assert_equal
        [ [ 9; 8; 7 ]; [ 32; 24; 16 ] ]
        (add_matrices [ [ 1; 1; 1 ]; [ 4; 3; 2 ] ] [ [ 9; 8; 7 ]; [ 8; 8; 8 ] ])
    );
    ( "shapes mismatch" >:: fun _ ->
      assert_raises (Invalid_argument "List.map2") (fun _ ->
          add_matrices [ [ 1; 1 ] ] [ [ 9; 8; 7 ] ]) );
  ]

let _ =
  "Exercise: matrix multiply [★★★★]" >::: test_add_matrices |> run_test_tt_main
