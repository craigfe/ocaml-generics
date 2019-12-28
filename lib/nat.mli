type z
(** zero *)

type +'n s
 (** successor *)

type +'i t
(** the type of natural numbers *)

val zero : ('n * 'n) t
val succ : ('n * 'm) t -> ('n s * 'm) t
val add : ('n * 'm) t -> ('m * 'l) t -> ('n * 'l) t
val to_int : _ t -> int
