# High-Order Programming

High-order functions either take other functions as input or return other functions as output (or both).
High-order functions are also knowns as functionals (not functors).
High-order functions enales beatiful, general, reusable code.

## [Warmup](./warmup.ml)

### The Abstraction Principle

The Abstraction Principle says to avoid requiring something to be stated more than once; instead, factor out the recurring pattern.

Here are some more relatively simple examples.

**Apply**: applies its first input to its second input

```ocaml
let apply f x = f x
```

**Pipeline**: applies its second input to its first input

```ocaml
let pipeline x f = f x
let ( |> ) = pipeline
```

**Compose**: composes two other functions

```ocaml
let compose f g x = f (g x)
```

**Both**: applies two functions to the same argument and returns a pair of the result

```ocaml
let both f g x = (f x, g x)
```

**Cond**: conditionally chooses which of two functions to apply based on the predicate

```ocaml
let cond p f g x = if p then f x else g x
```

## [Map](./map.ml)

It is not a correct implementation of `map`.

```ocaml
let rec map f = function [] -> [] | h :: t -> f h :: map f t
```

### Side Effects

Look back at the implementation of our `map` function. The evaluation of `map` occurs right to left in fact.
In the example below, `2\n1\n` will be printed in the terminal.

```ocaml
let p x = print_int x; print_newline(); x + 1
let lst = map p[1; 2]
```

In order to preserve the evaluation order, we have to implement `map` as this

```ocaml
let rec map = function [] -> [] | h :: t -> let h' = f h in h' :: map f t
```

The lesson is that if the evaluation order matters, we could use `let` expression to ensure it.

### Tail Recursion

```ocaml
let rev_map f =
  let rec rev_map_aux acc = function
    | [] -> acc
    | h :: t -> rev_map_aux (f h :: acc) t
  in
  rev_map_aux []

let map f lst = lst |> rev_map f |> List.rev
```

## [Filter](./filter.ml)

```ocaml
let rec filter p = function
  | [] -> []
  | h :: t -> if p h then h :: filter p t else filter p t
```

### Tail Recursion

```ocaml
let filter p =
  let rec filter_aux acc = function
    | [] -> List.rev acc
    | h :: t ->  if p h then filter_aux (h :: acc) t else filter_aux acc t
  in
  filter_aux []
```

## [Fold](./fold.ml)

### Combine

```ocaml
let rec combine op init = function
  | [] -> init
  | h :: t -> op h (combine op init t)
```

### Fold Right

```ocaml
let rec fold_right f lst acc =
  match lst with [] -> acc | h :: t -> f h (fold_right f t acc)
```

The `fold_right` folds in elements of the list from right to left.
For example, `fold_right ( + ) [1; 2; 3] 0` results in `1 + (2 + (3 + 0))`.
The paratheses associate from right to left.

### Tail Recursion and Combine

```ocaml
let rec combine_tr f acc = function
  | [] -> acc
  | h :: t -> combine_tr f (f acc h) t (* Not the order of [acc] and [h] *)
```

If we execute the following code snappet:

```ocaml
let s = combine ( - ) 0 [3; 2; 1]
let s' = combine_tr ( - ) 0 [3; 2; 1]
```

We will get

```ocaml
var s : int = 2;
var s' : int = -6;
```

- With `combine` we compute `(3 - (2 - (1 - 0)))`
- WIth `combine_tr` we compute `(((0 - 3) - 2) - 1)`

### Fold Left

In fact, the `combine_tr` is the `fold_left` we need.

```ocaml
let fold_left f acc = function
  | [] -> acc
  | h :: t -> fold_left f (f acc h) t
```

### Fold Left vs. Fold Right

- They combine list elements in opposite orders, as indicated by their names. Function fold_right combines from the right to the left, whereas fold_left proceeds from the left to the right.
- Function fold_left is tail recursive whereas fold_right is not.
- The types of the functions are different. In fold_X the accumulator argument goes to the X of the list argument. That is a choice made by the standard library rather than a necessary implementation difference.

## [Currying](./curry.ml)

- Curried function: `'a -> 'b -> 'c`
- Uncurried function: `'a * 'b -> 'c`

The way to convert between both of them:

```ocaml
let add x y = x + y
let add' t = fst t + snd t

(** [curry] takes an uncurried function and returns a curried function *)
let curry f x y = f (x, y)

(** [uncurry] takes a curried function and returns an uncurried function *)
let uncurry f (x, y) = f x y

let curried_add = curry add'
let uncurried_add = uncurry add
```
