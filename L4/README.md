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
