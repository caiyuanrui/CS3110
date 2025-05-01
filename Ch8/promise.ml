module type PROMISE = sig
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn
  type 'a promise
  type 'a resolver

  val make : unit -> 'a promise * 'a resolver
  val return : 'a -> 'a promise

  val state : 'a promise -> 'a state
  (** [state p] is the state of the promise. *)

  val fulfill : 'a resolver -> 'a -> unit
  (** [fultill r x] fulfills the promise [p] associated with [r] with value [x],
      meaning that [state p] will become [Fulfilled x]. Requires: [p] is
      pending. *)

  val rejected : 'a resolver -> exn -> unit
  (** [reject r x] rejects the promise [p] associated with [r] with exception
      [x], meaning that [state p] will become [Rejected x]. Requires: [p] is
      pending. *)
end

module Promise : PROMISE = struct
  type 'a state = Pending | Fulfilled of 'a | Rejected of exn
  type 'a promise = 'a state ref
  type 'a resolver = 'a promise

  let write_once p s =
    if !p = Pending then p := s else invalid_arg "cannot write twice"

  let make =
   fun () ->
    let p = ref Pending in
    (p, p)

  let return x = ref (Fulfilled x)
  let state p = !p
  let fulfill r x = write_once r (Fulfilled x)
  let rejected r x = write_once r (Rejected x)
end

open Lwt_io

let p =
  Lwt.bind (read_line stdin) (fun s1 ->
      Lwt.bind (read_line stdin) (fun s2 -> Lwt_io.printf "%s\n" (s1 ^ s2)))

let _ = Lwt_main.run p
