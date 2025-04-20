type pointer = { x : int; y : int; mutable c : string }

let p = { x = 0; y = 0; c = "red" }
let _ = p.c <- "green"

type 'a ref = { mutable contents : 'a }
