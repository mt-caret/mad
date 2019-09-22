open Core_kernel

type 'a t [@@deriving sexp]

include Quickcheck.S1 with type 'a t := 'a t

val equal : 'a t -> 'a t -> equal:('a -> 'a -> bool) -> bool

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> unit

val push : 'a t -> 'a -> unit

val create : unit -> 'a t

val length : 'a t -> int