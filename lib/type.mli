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

type ('record, 'field) field

type ('record, 'cons, 'remaining, 'lenses, 'lens_nil) open_record

val field : string -> 'field t -> ('record -> 'field) -> ('record, 'field) field

val sealr : ('record, 'cons, 'record, 'lens, unit) open_record -> 'record t

val sealr_lens :
  ('record, 'cons, 'record, 'lens, unit) open_record ->
  'record t * 'lens Optics.Lens.t_list

val ( |+ ) :
  ( 'record,
    'cons,
    'field -> 'remaining_fields,
    'lens,
    ('record, 'field) Optics.Lens.mono * 'lens_nil )
  open_record ->
  ('record, 'field) field ->
  ('record, 'cons, 'remaining_fields, 'lens, 'lens_nil) open_record

val record :
  string -> 'record -> ('a, 'record, 'record, 'lens_nil, 'lens_nil) open_record

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
