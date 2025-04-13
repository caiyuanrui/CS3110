let sum_sq n =
  let rec loop i sum = if i > n then sum else loop (i + 1) (sum + (i * i)) in
  loop 0 0

let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)
let square x = x * x
let sum = List.fold_left ( + ) 0
let sum_sq n = 0 -- n |> List.map square |> sum
