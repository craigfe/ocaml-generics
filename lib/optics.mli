(** Optics in OCaml *)

module Lens : sig
  type (-'s, +'t, +'a, -'b) t
  (** The type of lenses, where the type parameters are as follows:

      - ['s]: the source type
      - ['t]: the output type
      - ['a]: the focus of the lens
      - ['b]: is the type such that ['b/'a]'s = 't, i.e. the result type of the
        focused transformation that produces the necessary output type 't. *)

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
        (** Convenient syntax for a heterogeneous list of lenses. *)

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t
  (** The type of monomorphic lenses (lenses such that the internal transform is
      type-preserving) *)

  val v : ('s -> 'a) -> ('s -> 'b -> 't) -> ('s, 't, 'a, 'b) t
  (** [v f m] is the lens given by the focusing function [f] and the
      modification function [m]. *)

  val view : ('s, 't, 'a, 'b) t -> 's -> 'a
  (** See the focus of a lens *)

  val update : ('s, 't, 'a, 'b) t -> 'b -> 's -> 't
  (** [update l b s] replaces the focus of [l] in [s] with [b]. *)

  val modify : ('s, 't, 'a, 'b) t -> ('a -> 'b) -> 's -> 't
  (** [modify l f s] is the result of applying [f] to the focus of [l] in [s]. *)

  val ( >> ) : ('a, 'b, 'c, 'd) t -> ('c, 'd, 'e, 'f) t -> ('a, 'b, 'e, 'f) t
  (** [l1 >> l2] is the composition of lenses l1 and l2. *)

  (** {3 Common lenses} *)

  val id : ('a, 'a) mono

  val fst : ('a1 * 'b, 'a2 * 'b, 'a1, 'a2) t

  val snd : ('a * 'b1, 'a * 'b2, 'b1, 'b2) t

  val head : ('a list, 'a) mono
end

module Prism : sig
  type (-'s, +'t, +'a, -'b) t
  (** The type of prisms.

      The prism is the categorical dual of the lens (it operates on sum types
      where lenses operate on product types). As such, the access function is
      non-total *)

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  type _ t_list =
    | ( :: ) :
        ('s, 't, 'a, 'b) t * 'l t_list
        -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list

  val v : ('b -> 't) -> ('s -> ('a, 't) result) -> ('s, 't, 'a, 'b) t

  val ( >> ) : ('a, 'b, 'c, 'd) t -> ('c, 'd, 'e, 'f) t -> ('a, 'b, 'e, 'f) t
  (** [l1 >> l2] is the composition of prisms l1 and l2. *)

  (** {3 Common prisms} *)

  (* val some : ('a option, 'b option, 'a, 'c) t
   * 
   * val none : ('a option, unit) mono
   * 
   * val ok : (('a, 'c) result, ('b, 'c) result, 'a, 'b) t
   * 
   * val error : (('a, 'b) result, ('a, 'c) result, 'b, 'c) t
   * 
   * val head : ('a list, 'a) mono
   * 
   * val nil : ('a list, unit) mono *)
end
