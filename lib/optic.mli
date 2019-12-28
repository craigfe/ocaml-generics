(** Optics in OCaml *)

type z
type +'i s

type (-'s, +'t, +'a, -'b, +_) t
(** The type of optics, where the type parameters are as follows:

    - ['s]: the source type
    - ['t]: the output type
    - ['a]: the focus of the lens
    - ['b]: is the type such that ['b/'a]'s = 't, i.e. the result type of the
      focused transformation that produces the necessary output type 't. *)

type ('s, 'a, 'safety) mono = ('s, 's, 'a, 'a, 'safety) t
(** The type of monomorphic optics (optics such that the internal transform is
    type-preserving). *)

type _ t_list =
  | ( :: ) : ('s, 't, 'a, 'b, 'typ) t * 'l t_list -> (('s, 't, 'a, 'b, 'typ) t * 'l) t_list
  | [] : unit t_list
(** Convenient syntax for a heterogeneous list of optics. *)

val lens : ('s -> 'a) -> ('s -> 'b -> 't) -> ('s, 't, 'a, 'b, ('z * 'z)) t
(** [lens f m] is the lens given by the focusing function [f] and the
    modification function [m]. *)

val prism : ('b -> 't) * ('s -> ('a, 't) result) -> ('s, 't, 'a, 'b, ('z s * 'z)) t

(** {3 Stock lenses} *)

val id : ('a, 'a, ('z * 'z)) mono

val fst : ('a1 * 'b, 'a2 * 'b, 'a1, 'a2, ('z * 'z)) t

val snd : ('a * 'b1, 'a * 'b2, 'b1, 'b2, ('z * 'z)) t

val head : ('a list, 'a, ('z * 'z)) mono

val view : ('s, 't, 'a, 'b, ('n * 'n)) t -> 's -> 'a
(** See the focus of a lens *)

val view_opt : ('s, 't, 'a, 'b, (_ s * z)) t -> 's -> 'a

val update : ('s, 't, 'a, 'b, ('n * 'n)) t -> 'b -> 's -> 't
(** [update l b s] replaces the focus of [l] in [s] with [b]. *)

val update_opt : ('s, 't, 'a, 'b, (_ s * z)) t

val modify : ('s, 't, 'a, 'b, ('n * 'n)) t -> ('a -> 'b) -> 's -> 't
(** [modify l f s] is the result of applying [f] to the focus of [l] in [s]. *)

val ( >> ) : ('a, 'b, 'c, 'd, ('n * 'm)) t -> ('c, 'd, 'e, 'f, ('m * 'l)) t -> ('a, 'b, 'e, 'f, ('n * 'l)) t
(** [l1 >> l2] is the composition of optics l1 and l2. *)

(* {1. Derive lenses from generics } *)
(* type ('name, 's, 't, 'a, 'b) has_field
 * 
 * val field : ('name, 's, 't, 'a, 'b) has_field -> ('s, 't, 'a, 'b) t
 * 
 * val typed : ('a, 's) has_type -> ('s, 's, 'a, 'a) t *)


(* val head: ('a list, 'a,) mono *)
