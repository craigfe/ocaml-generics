type 'a t
(** The type of generics *)

(* Contravariant functor instance *)
(* val fmap : 'a t -> ('b -> 'a) -> 'b t *)

(** Defines the properties a Type-able type must satisfy *)

val to_string : 'a t -> 'a -> string

val compare : 'a t -> 'a -> 'a -> int

(** Derivable from the above *)

val hash : 'a t -> 'a -> (string -> unit) -> unit

val pp : 'a t -> 'a Fmt.t

val equal : 'a t -> 'a -> 'a -> bool

(** Basic generic constructors *)

val unit : unit t

val bool : bool t

val int : int t

val float : float t

val string : string t

val bytes : bytes t

val list : 'a t -> 'a list t

(** Record generic constructors *)

type ('a, 'b) field

type ('a, 'b, 'c) open_record

val field : string -> 'a t -> ('b -> 'a) -> ('b, 'a) field

val sealr : ('a, 'b, 'a) open_record -> 'a t

val ( |+ ) :
  ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record

val record : string -> 'b -> ('a, 'b, 'b) open_record

(** Variant generic constructors *)

type ('a, 'b) case

type 'a case_p

type ('a, 'b, 'c) open_variant

val case0 : string -> 'a -> ('a, 'a case_p) case

val case1 : string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case

val sealv : ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t

val variant : string -> 'b -> ('a, 'b, 'b) open_variant

val ( |~ ) :
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant
