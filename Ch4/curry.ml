let add x y = x + y
let add' t = fst t + snd t

(** [curry] takes an uncurried function and returns a curried function *)
let curry f x y = f (x, y)

(** [uncurry] takes a curried function and returns an uncurried function *)
let uncurry f (x, y) = f x y

let curried_add = curry add'
let uncurried_add = uncurry add
