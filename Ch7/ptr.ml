type 'a pointer = 'a ref option

let null : 'a pointer = None
let malloc (x : 'a) : 'a pointer = Some (ref x)

exception Segfault

let deref (ptr : 'a pointer) : 'a =
  match ptr with None -> raise Segfault | Some r -> !r

let ( ~* ) = deref
let p = malloc 42
let v = ~*p

let assign (ptr : 'a pointer) (x : 'a) : unit =
  match ptr with None -> raise Segfault | Some r -> r := x

let ( =* ) = assign

let address (ptr : 'a pointer) : int =
  match ptr with None -> raise Segfault | Some r -> Obj.magic r

let ( ~& ) = address
