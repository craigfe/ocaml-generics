(** Lenses *)

type (-'s, +'t, +'a, -'b) t
(** The type of lenses, where the type parameters are as follows:

    - ['s]: the source type
    - ['t]: the output type
    - ['a]: the focus of the lens
    - ['b]: is the type such that ['b/'a]'s = 't, i.e. the result type of the
      focused transformation that produces the necessary output type 't. *)

val lens : ('s -> 'a) -> ('s -> 'b -> 't) -> ('s, 't, 'a, 'b) t
(** [lens f m] is the lens given by the focusing function [f] and the
    modification function [m]. *)

val view : ('s, 't, 'a, 'b) t -> 's -> 'a
(** See the focus of a lens *)

val update : ('s, 't, 'a, 'b) t -> 'b -> 's -> 't
(** [update l b s] replaces the focus of [l] in [s] with [b]. *)

val modify : ('s, 't, 'a, 'b) t -> ('a -> 'b) -> 's -> 't
(** [modify l f s] is the result of applying [f] to the focus of [l] in [s]. *)

val ( >> ) : ('a, 'b, 'c, 'd) t -> ('d, 'e, 'f, 'g) t -> ('a, 'b, 'e, 'f) t
(** [l1 >> l2] is the composition of lenses l1 and l2. *)

(* {1. Derive lenses from generics } *)
(* type ('name, 's, 't, 'a, 'b) has_field
 * 
 * val field : ('name, 's, 't, 'a, 'b) has_field -> ('s, 't, 'a, 'b) t
 * 
 * val typed : ('a, 's) has_type -> ('s, 's, 'a, 'a) t *)
