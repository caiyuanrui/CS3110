open OUnit2

(* Exercise: patterns [★★★]

Using pattern matching, write three functions, one for each of the following properties. Your functions should return true if the input list has the property and false otherwise.

the list’s first element is "bigred"

the list has exactly two or four elements; do not use the length function

the first two elements of the list are equal *)

let bigred = function [] -> false | h :: _ -> h = "bigred"

let rec two_or_four = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let eq = function a :: b :: _ -> a = b | _ -> false

(* Exercise: library [★★★]

Consult the List standard library to solve these exercises:

Write a function that takes an int list and returns the fifth element of that list, if such an element exists. If the list has fewer than five elements, return 0. Hint: List.length and List.nth.

Write a function that takes an int list and returns the list sorted in descending order. Hint: List.sort with Stdlib.compare as its first argument, and List.rev. *)
let fifth_ele l = if List.length l < 5 then 0 else List.nth l 5
let desc l = List.sort Stdlib.compare l |> List.rev

(* Exercise: library test [★★★]

Write a couple OUnit unit tests for each of the functions you wrote in the previous exercise. *)

let rec string_of_int_list = function
  | [] -> ""
  | h :: t -> string_of_int h ^ " " ^ string_of_int_list t

let tests =
  "test suite for bigred"
  >::: [
         ("" >:: fun _ -> assert_bool "has bigred" (bigred [ "bigred" ]));
         ( "" >:: fun _ ->
           assert_bool "hasn't bigred"
             (bigred [ "smallgreen"; "unsense" ] |> not) );
         ("" >:: fun _ -> assert_bool "" (two_or_four [ 0; 0 ]));
         ("" >:: fun _ -> assert_bool "" (two_or_four [ 0; 0; 0; 0 ]));
         ("" >:: fun _ -> assert_bool "" (two_or_four [ 0 ] |> not));
         ("" >:: fun _ -> assert_bool "" (two_or_four [ 0; 0; 0 ] |> not));
         ("" >:: fun _ -> assert_bool "" (eq [ 0; 0 ]));
         ("" >:: fun _ -> assert_bool "" (eq [ 0; 0; 0 ]));
         ("" >:: fun _ -> assert_bool "" (eq [ 0; 1 ] |> not));
         ("" >:: fun _ -> assert_bool "" (eq [ 0 ] |> not));
         ( "" >:: fun _ ->
           assert_equal 42
             (fifth_ele [ 1; 2; 3; 4; 5; 42; 6 ])
             ~printer:string_of_int );
         ( "" >:: fun _ ->
           assert_equal 0 (fifth_ele [ 42; 42; 42; 42 ]) ~printer:string_of_int
         );
         ( "" >:: fun _ ->
           assert_equal [ 5; 4; 3; 2 ]
             (desc [ 3; 5; 4; 2 ])
             ~printer:string_of_int_list );
       ]

let _ = run_test_tt_main tests
(* Exercise: library puzzle [★★★]

Write a function that returns the last element of a list. Your function may assume that the list is non-empty. Hint: Use two library functions, and do not write any pattern matching code of your own.

Write a function any_zeros : int list -> bool that returns true if and only if the input list contains at least one 0. Hint: use one library function, and do not write any pattern matching code of your own.

Your solutions will be only one or two lines of code each. *)

let last_ele lst = List.nth lst (List.length lst - 1)
let any_zeros lst = List.find_opt (fun x -> x = 0) lst |> ( <> ) None

(* Exercise: take drop [★★★]

Write a function take : int -> 'a list -> 'a list such that take n lst returns the first n elements of lst. If lst has fewer than n elements, return all of them.

Write a function drop : int -> 'a list -> 'a list such that drop n lst returns all but the first n elements of lst. If lst has fewer than n elements, return the empty list. *)
let rec take n lst =
  match lst with
  | [] -> []
  | h :: t -> if n > 0 then h :: take (n - 1) t else []

let rec drop n lst =
  match lst with [] -> [] | _ when n = 0 -> lst | _ :: t -> drop (n - 1) t

let test_take take =
  [
    ( "take 3 from 5 elements" >:: fun _ ->
      assert_equal [ 1; 2; 3 ]
        (take 3 [ 1; 2; 3; 4; 5 ])
        ~printer:string_of_int_list );
    ( "take 1 from 5 elements" >:: fun _ ->
      assert_equal [ 1 ] (take 1 [ 1; 2; 3; 4; 5 ]) ~printer:string_of_int_list
    );
    ( "take 0 returns empty list" >:: fun _ ->
      assert_equal [] (take 0 [ 1; 2; 3; 4; 5 ]) ~printer:string_of_int_list );
    ( "take from empty list" >:: fun _ ->
      assert_equal [] (take 3 []) ~printer:string_of_int_list );
    ( "take more than length" >:: fun _ ->
      assert_equal [ 1; 2; 3 ] (take 5 [ 1; 2; 3 ]) ~printer:string_of_int_list
    );
  ]

let test_drop drop =
  [
    ( "drop 2 from 5 elements" >:: fun _ ->
      assert_equal [ 3; 4; 5 ]
        (drop 2 [ 1; 2; 3; 4; 5 ])
        ~printer:string_of_int_list );
    ( "drop 1 from 5 elements" >:: fun _ ->
      assert_equal [ 2; 3; 4; 5 ]
        (drop 1 [ 1; 2; 3; 4; 5 ])
        ~printer:string_of_int_list );
    ( "drop all elements" >:: fun _ ->
      assert_equal [] (drop 5 [ 1; 2; 3; 4; 5 ]) ~printer:string_of_int_list );
    ( "drop 0 keeps list" >:: fun _ ->
      assert_equal [ 1; 2; 3; 4; 5 ]
        (drop 0 [ 1; 2; 3; 4; 5 ])
        ~printer:string_of_int_list );
    ( "drop from empty list" >:: fun _ ->
      assert_equal [] (drop 3 []) ~printer:string_of_int_list );
  ]

let _ = "test_suite" >::: test_take take @ test_drop drop |> run_test_tt_main

(* Exercise: take drop tail [★★★★]

Revise your solutions for take and drop to be tail recursive, if they aren’t already. Test them on long lists with large values of n to see whether they run out of stack space. To construct long lists, use the -- operator from the lists section. *)
let rec from i j l = if i > j then l else from i (j - 1) (j :: l)
let ( -- ) i j = from i j []
(* let long_list = 1 -- 100_000_000 *)
(* let _ = take 100_000_000 long_list
let _ = drop 100_000_000 long_list *)

let take n lst =
  let rec take_aux n lst acc =
    match lst with
    | [] -> List.rev acc
    | _ when n = 0 -> List.rev acc
    | h :: t -> take_aux (n - 1) t (h :: acc)
  in
  take_aux n lst []

(* drop is already tail recursive *)

let _ =
  "tail recursive" >::: test_take take @ test_drop drop |> run_test_tt_main

(* Exercise: unimodal [★★★]

Write a function is_unimodal : int list -> bool that takes an integer list and returns whether that list is unimodal. A unimodal list is a list that monotonically increases to some maximum value then monotonically decreases after that value. Either or both segments (increasing or decreasing) may be empty. A constant list is unimodal, as is the empty list. *)
let is_unimodal lst =
  let rec aux lst last =
    match lst with
    | [] -> true
    | h :: t -> (
        match last with
        | None, _ -> aux t (Some h, true)
        | Some last, asc ->
            if (not asc) && h > last then false
            else aux t (Some h, asc && h >= last))
  in
  aux lst (None, true)

let test_is_unimodal is_unimodal =
  [
    ( "normal unimodal list" >:: fun _ ->
      assert_bool "should succeed" (is_unimodal [ 1; 2; 3; 4; 3; 2; 1 ]) );
    ("empty list" >:: fun _ -> assert_bool "should succeed" (is_unimodal []));
    ( "identity list" >:: fun _ ->
      assert_bool "should succeed" (is_unimodal [ 1; 1; 1 ]) );
    ( "not unimodal list" >:: fun _ ->
      assert_bool "should fail" (is_unimodal [ 1; 2; 1; 2 ] |> not) );
  ]

let _ = "test is_unimodal" >::: test_is_unimodal is_unimodal |> run_test_tt_main

(* Exercise: powerset [★★★]

Write a function powerset : int list -> int list list that takes a set S represented as a list and returns the set of all subsets of S. The order of subsets in the powerset and the order of elements in the subsets do not matter.

Hint: Consider the recursive structure of this problem. Suppose you already have p, such that p = powerset s. How could you use p to compute powerset (x :: s)? *)
let rec powerset s =
  match s with
  | [] -> [ [] ]
  | h :: t ->
      let subset = powerset t in
      subset @ List.map (fun subset -> h :: subset) subset

(* Exercise: pokefun [★★★]

Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the Pokémon with the highest HP. *)
type pokemon = { hp : int }

let rec max_hp = function
  | [] -> None
  | [ p ] -> Some p.hp
  | h :: t ->
      Some
        (max h.hp
           (match max_hp t with None -> failwith "unreachable" | Some h -> h))

(* Exercise: date before [★★]

Define a date-like triple to be a value of type int * int * int. Examples of date-like triples include (2013, 2, 1) and (0, 0, 1000). A date is a date-like triple whose first part is a positive year (i.e., a year in the common era), second part is a month between 1 and 12, and third part is a day between 1 and 31 (or 30, 29, or 28, depending on the month and year). (2013, 2, 1) is a date; (0, 0, 1000) is not.

Write a function is_before that takes two dates as input and evaluates to true or false. It evaluates to true if the first argument is a date that comes before the second argument. (If the two dates are the same, the result is false.)

Your function needs to work correctly only for dates, not for arbitrary date-like triples. However, you will probably find it easier to write your solution if you think about making it work for arbitrary date-like triples. For example, it’s easier to forget about whether the input is truly a date, and simply write a function that claims (for example) that January 100, 2013 comes before February 34, 2013—because any date in January comes before any date in February, but a function that says that January 100, 2013 comes after February 34, 2013 is also valid. You may ignore leap years. *)

let is_date = function _ -> true

let is_before (y1, m1, d1) (y2, m2, d2) =
  if (is_date (y1, m1, d1) && is_date (y2, m2, d2)) |> not then false
  else if y1 < y2 then true
  else if y1 > y2 then false
  else if m1 < m2 then true
  else if m1 > m2 then false
  else if d1 < d2 then true
  else false

(* Exercise: earliest date [★★★]

Write a function earliest : (int*int*int) list -> (int * int * int) option. It evaluates to None if the input list is empty, and to Some d if date d is the earliest date in the list. Hint: use is_before.

As in the previous exercise, your function needs to work correctly only for dates, not for arbitrary date-like triples. *)
let rec earliest = function
  | [] -> None
  | h :: t -> (
      let other = earliest t in
      match other with
      | None -> Some h
      | Some o -> if is_before h o then Some h else Some o)

(* Exercise: depth [★★]

Write a function depth : 'a tree -> int that returns the number of nodes in any longest path from the root to a leaf. For example, the depth of an empty tree (simply Leaf) is 0, and the depth of tree t above is 3. Hint: there is a library function max : 'a -> 'a -> 'a that returns the maximum of any two values of the same type. *)

type 'a tree = Leaf | Node of 'a node
and 'a node = { value : 'a; left : 'a tree; right : 'a tree }

let rec depth = function
  | Leaf -> 0
  | Node { value; left; right } -> 1 + max (depth left) (depth right)

(* tail recursive *)
let depth tr =
  let rec depth_aux acc = function
    | Leaf -> acc
    | Node { left; right; _ } ->
        let left_depth = depth_aux (acc + 1) left in
        let right_depth = depth_aux (acc + 1) right in
        max left_depth right_depth
  in
  depth_aux 0 tr

(* Exercise: is_bst [★★★★]

Write a function is_bst : ('a*'b) tree -> bool that returns true if and only if the given tree satisfies the binary search tree invariant. An efficient version of this function that visits each node at most once is somewhat tricky to write. Hint: write a recursive helper function that takes a tree and either gives you (i) the minimum and maximum value in the tree, or (ii) tells you that the tree is empty, or (iii) tells you that the tree does not satisfy the invariant. Your is_bst function will not be recursive, but will call your helper function and pattern match on the result. You will need to define a new variant type for the return type of your helper function. *)

let rec is_bst tr =
  let rec helper v left right =
    (match left with
    | Leaf -> true
    | Node { value; left; right } -> v > value && helper value left right)
    &&
    match right with
    | Leaf -> true
    | Node { value; left; right } -> v < value && helper value left right
  in
  match tr with
  | Leaf -> true
  | Node { value; left; right } -> helper value left right

(* Unit tests *)
let test_is_bst () =
  let valid_bst =
    Node
      {
        value = 10;
        left = Node { value = 5; left = Leaf; right = Leaf };
        right = Node { value = 15; left = Leaf; right = Leaf };
      }
  in
  let invalid_bst =
    Node
      {
        value = 10;
        left = Node { value = 15; left = Leaf; right = Leaf };
        right = Node { value = 5; left = Leaf; right = Leaf };
      }
  in
  let empty_tree = Leaf in
  let single_node = Node { value = 10; left = Leaf; right = Leaf } in
  let complex_tree =
    Node
      {
        value = 10;
        left =
          Node
            {
              value = 5;
              left =
                Node
                  {
                    value = 3;
                    left = Node { value = 2; left = Leaf; right = Leaf };
                    right = Node { value = 4; left = Leaf; right = Leaf };
                  };
              right = Leaf;
            };
        right = Node { value = 15; left = Leaf; right = Leaf };
      }
  in

  let test_cases =
    [
      ("Valid BST" >:: fun _ -> assert_equal true (is_bst valid_bst));
      ("Invalid BST" >:: fun _ -> assert_equal false (is_bst invalid_bst));
      ("Empty tree" >:: fun _ -> assert_equal true (is_bst empty_tree));
      ("Single node tree" >:: fun _ -> assert_equal true (is_bst single_node));
      ("Complex tree" >:: fun _ -> assert_equal true (is_bst complex_tree));
    ]
  in

  run_test_tt_main ("is_bst tests" >::: test_cases)

(* Run the tests *)
let () = test_is_bst ()
