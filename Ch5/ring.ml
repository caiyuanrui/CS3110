module type Ring = sig
  type t

  val zero : t
  val one : t
  val ( + ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( ~- ) : t -> t
  val to_string : t -> string
end

module IntRing : Ring = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

let two : IntRing.t = IntRing.(one + one)

module IntRing' : Ring with type t = int = struct
  type t = int

  let zero = 0
  let one = 1
  let ( + ) = Stdlib.( + )
  let ( * ) = Stdlib.( * )
  let ( ~- ) = Stdlib.( ~- )
  let to_string = string_of_int
end

let two : int = IntRing'.(one + one)
