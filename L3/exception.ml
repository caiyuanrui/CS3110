(* define an exception *)
exception E of int

(* built-in exception type *)
let e = Failure ""

(* raise an exception value *)
let _ = raise e

(* equivalent to raise Failure s *)
let _ = failwith ""

let _ =
  try if true then failwith "failure" else 42 with
  | Failure s ->
      print_string s;
      42
  | E i ->
      print_int i;
      42

(* Pattern matching *)
let _ =
  match List.hd [] with
  | [] -> "empty"
  | _ :: _ -> "non-empty"
  | exception Failure s -> s

(* Desugar *)
let _ =
  try match List.hd [] with [] -> "empty" | _ :: _ -> "non-empty"
  with Failure s -> s

(* Exception and Ounit *)
open OUnit2

let tests =
  "suite"
  >::: [
         ( "empty" >:: fun _ ->
           assert_raises (Failure "hd") (fun () -> List.hd []) );
       ]

let _ = run_test_tt_main tests
