open Optics

let undefined _ =
  let exception Undefined in
  raise Undefined

type 'a t =
  | Unit : unit t
  | Bool : bool t
  | Bytes : bytes t
  | String : string t
  | Float : float t
  | Int : int t
  | List : 'a t -> 'a list t
  | Record : 'a record -> 'a t
  | Variant : 'a variant -> 'a t

(* Records *)
and 'a record = {
  rwit : 'a Witness.t;
  rname : string;
  rfields : 'a fields_and_constr;
}

and 'a fields_and_constr =
  | Fields : ('a, 'b, _, _) fields * 'b -> 'a fields_and_constr

and ('a, _, 'lens, _) fields =
  | F0 : ('a, 'a, 'lens, 'lens) fields
  | F1 :
      ('a, 'b) field * ('a, 'c, 'lens, 'lens_nil) fields
      -> ('a, 'b -> 'c, ('a, 'b) Lens.mono * 'lens, 'lens_nil) fields
      (** We need to accumulate two variants of 'type-level list' here...
          Without higher-kinded type operators, we must explicitly accumulate
          all types that we will need here (otherwise, we could express as
          higher-order F-algebra and later tie the loop with the appropriate
          kinds and types).

          {[
            e := bin (t, e) | nil, for some

            - (bin :: !a -> !b -> !b)
            - (nil :: !a)
          ]}

          The type parameters [('a, 'b, 'c)] have the following meanings:

          - ['a] : the type of the record under construction.

          - ['b] : function type consuming all of the fields passed so far and
            returning 'a, i.e. f-algebra above with [bin l r] := [l -> r] and
            [nil] := ['a]. This gradually builds the type of the constructor
            function, so that the variant can be sealed when this is equal to
            the supplied constructor type.

          - ['c] : heterogeneous list of lenses for the fields passed so far,
            i.e. the f-algebra with [bin l r] := [('a, l) Lens.mono * r] and
            [nil] := [unit].

          For example, given a record of type [foo] with fields of type [int]
          and [string], the type parameters are as follows:

          {[
            ('a, 'b, 'c)  =  (foo,     ->         ,          *      , 'd)
                                      /  \                 /   \
                                     /    \               /     \
                                    /      \             /       \
                                  int      ->     (foo, int)     *
                                          /  \     Lens.mono    /   \
                                         /    \                /     \
                                        /      \              /       \
                                     string    foo     (foo, string)  'd
                                                         Lens.mono
          ]} *)

and (-'record, 'field) field = {
  fname : string;
  ftype : 'field t;
  fget : 'record -> 'field;
}

(* Variants *)
and 'a variant = {
  vwit : 'a Witness.t;
  vname : string;
  vcases : 'a a_case array;
  vget : 'a -> 'a case_v;
}

and 'a a_case = C0 : 'a case0 -> 'a a_case | C1 : ('a, 'b) case1 -> 'a a_case

and 'a case_v =
  | CV0 : 'a case0 -> 'a case_v
  | CV1 : ('a, 'b) case1 * 'b -> 'a case_v

and 'a case0 = { ctag0 : int; cname0 : string; c0 : 'a }

and ('a, 'b) case1 = {
  ctag1 : int;
  cname1 : string;
  ctype1 : 'b t;
  c1 : 'b -> 'a;
}

(* Polymorphic map over fields over a record. Sadly, without higher-kinded types
   we have to hand implement each one. *)
type ('record, 'out) polyf = { f_field : 'a. ('record, 'a) field -> 'out }

let rec map_fields :
    type record out fun_tr list_tr lens_hole.
    (record, out) polyf ->
    (record, fun_tr, list_tr, lens_hole) fields ->
    out list =
 fun func -> function
  | F0 -> []
  | F1 (field, fields) -> func.f_field field :: map_fields func fields

let unit = Unit

let bool = Bool

let bytes = Bytes

let string = String

let float = Float

let int = Int

let list elt = List elt

(* Variants *)

type 'a case_p = 'a case_v

type ('a, 'b) case = int -> 'a a_case * 'b

let case0 cname0 c0 ctag0 =
  let c = { ctag0; cname0; c0 } in
  (C0 c, CV0 c)

let case1 cname1 ctype1 c1 ctag1 =
  let c = { ctag1; cname1; ctype1; c1 } in
  (C1 c, fun v -> CV1 (c, v))

type ('a, 'b, 'c) open_variant = 'a a_case list -> string * 'c * 'a a_case list

let variant n c vs = (n, c, vs)

let sealv v =
  let vname, vget, vcases = v [] in
  let vwit = Witness.make () in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vname; vcases; vget }

let ( |~ ) v c cs =
  let n, fc, cs = v cs in
  let c, f = c (List.length cs) in
  (n, fc f, c :: cs)

(* Records *)

module Unwitnessed_record = struct
  type ('record, 'cons, 'lens, 'lens_nil) t = {
    name : string;
    cons : 'cons;
    fields : ('record, 'cons, 'lens, 'lens_nil) fields;
  }

  let v name cons fields = { name; cons; fields }
end

type ('record, 'cons, 'remaining, 'lenses, 'lens_nil) open_record = {
  open_record :
    'hole. ('record, 'remaining, 'lens_nil, 'hole) fields ->
    (* Append the two lens lists at the type level *)
    ('record, 'cons, 'lenses, 'hole) Unwitnessed_record.t;
}

let field fname ftype fget = { fname; ftype; fget }

let record : type r. string -> r -> ('a, r, r, 'lens_nil, 'lens_nil) open_record
    =
 fun n r ->
  let open_record fs = Unwitnessed_record.v n r fs in
  { open_record }

let app :
    type r c ft rem lens lens_nil.
    (r, c, ft -> rem, lens, (r, ft) Lens.mono * lens_nil) open_record ->
    (r, ft) field ->
    (r, c, rem, lens, lens_nil) open_record =
 fun { open_record = previous } field ->
  let open_record' :
      type hole.
      (r, rem, lens_nil, hole) fields -> (r, c, lens, hole) Unwitnessed_record.t
      =
   fun fs -> previous (F1 (field, fs))
  in
  { open_record = open_record' }

(* fun { open_record = o } f -> { open_record = fun fs -> o (F1 (f, fs)) } *)

let sealr : type a b. (a, b, a, _, _) open_record -> a t =
 fun { open_record = r } ->
  let Unwitnessed_record.{ name; cons; fields } = r F0 in
  let rwit = Witness.make () in
  Record { rwit; rname = name; rfields = Fields (fields, cons) }

(* Ground constructor difference list with [record] and lens list with [unit] *)
let sealr_lens :
    type record cons lens.
    (record, cons, record, lens, unit) open_record ->
    record t * lens Lens.t_list =
 fun { open_record = r } ->
  let Unwitnessed_record.{ name; cons; fields } = r F0 in
  let rwit = Witness.make () in
  let lenses =
    let open Lens in
    let rec inner : type a l. (record, a, l, unit) fields -> l Lens.t_list =
      function
      | F0 -> []
      | F1 ({ fget; _ }, fs) ->
          let ml = Lens.v fget (fun _ _ -> assert false) in
          ml :: inner fs
    in
    inner fields
  in
  (Record { rwit; rname = name; rfields = Fields (fields, cons) }, lenses)

let ( |+ ) = app

(* Generic functions *)

let rec compare : type a. a t -> a -> a -> int = function
  | Unit -> Unit.compare
  | Bool -> Bool.compare
  | Bytes -> Bytes.compare
  | String -> String.compare
  | Float -> Float.compare
  | Int -> Int.compare
  | List elt ->
      let rec aux x y =
        match (x, y) with
        | [], [] -> 0
        | [], _ -> -1
        | _, [] -> 1
        | x :: xs, y :: ys -> (
            match compare elt x y with 0 -> aux xs ys | i -> i )
      in
      aux
  | Record _ -> undefined
  | Variant _ -> undefined

let equal typ a b = compare typ a b = 0

let rec pp : type a. a t -> a Fmt.t =
  let open Fmt in
  function
  | Unit -> unit "()"
  | Bool -> bool
  | Bytes -> using Bytes.unsafe_to_string string
  | String -> string
  | Float -> float
  | Int -> int
  | List elt -> brackets (list ~sep:comma (pp elt))
  | Record { rfields = Fields (fs, _cons); _ } ->
      fun ppf r ->
        Fmt.pf ppf "{\n";
        fs
        |> map_fields
             {
               f_field =
                 (fun { fname; ftype; fget } ->
                   Fmt.pf ppf "  %s = %a;\n" fname (pp ftype) (fget r));
             }
        |> (ignore : unit list -> unit);
        Fmt.pf ppf "}"
  | Variant _ -> undefined

let hash = undefined

let to_string typ = Fmt.str "%a" (pp typ)
