type (_, _) eq = Refl : ('a, 'a) eq

type 'a t

val make : unit -> 'a t

val eq : 'a t -> 'b t -> ('a, 'b) eq option
