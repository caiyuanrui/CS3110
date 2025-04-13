let apply f x = f x
let pipeline x f = f x
let ( |> ) = pipeline
let compose f g x = x |> g |> f
let both f g x = (f x, g x)
let cond p f g x = if p then f x else g x
