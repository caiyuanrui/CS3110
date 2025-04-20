let rec fact_rec n = if n = 0 then 1 else n * fact_rec (n - 1)

(* tying the recursive knot *)
let fact0 = ref (fun x -> x + 0)
let fact n = if n = 0 then 1 else n * !fact0 (n - 1)
let _ = fact0 := fact
let id = ref (fun x -> x)
