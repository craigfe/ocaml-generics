let undefined _ = assert false

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
  | Fields : ('a, 'b) fields * 'b -> 'a fields_and_constr

and ('a, 'b) fields =
  | F0 : ('a, 'a) fields
  | F1 : ('a, 'b) field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

and ('a, 'b) field = { fname : string; ftype : 'b t; fget : 'a -> 'b }

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

type ('a, 'b, 'c) open_record = ('a, 'c) fields -> string * 'b * ('a, 'b) fields

let field fname ftype fget = { fname; ftype; fget }

let record : string -> 'b -> ('a, 'b, 'b) open_record = fun n c fs -> (n, c, fs)

let app :
    type a b c d.
    (a, b, c -> d) open_record -> (a, c) field -> (a, b, d) open_record =
 fun r f fs ->
  let n, c, fs = r (F1 (f, fs)) in
  (n, c, fs)

let sealr : type a b. (a, b, a) open_record -> a t =
 fun r ->
  let rname, c, fs = r F0 in
  let rwit = Witness.make () in
  Record { rwit; rname; rfields = Fields (fs, c) }

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
  | Record _ -> undefined
  | Variant _ -> undefined

let hash = undefined

let to_string typ = Fmt.str "%a" (pp typ)
