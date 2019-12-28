(** {1 Optics in OCaml}

We use an optic representation very similar to the standard 'van Laarhoven'
encoding (based on CPS):

{[
  type (-'s, +'t, +'a, -'b) t = {
    op : 'r. ('a -> ('b -> 'r) -> 'r) -> 's -> ('t -> 'r) -> 'r;
  }
]}

However, the above quickly breaks down when used in practice, because of the
value restriction. In particular, the corresponding definition of [lens] is:

{[
  let lens : type s t a b. (s -> a) -> (s -> b -> t) -> (s, t, a, b) lens =
    fun get set ->
      let op k this read = k (get this) (fun b -> read (set this b)) in
      { op }
]}

which partially applies the '

Instead, we do type-level eta expansion.

@see https://stackoverflow.com/questions/29187287/sneaking-lenses-and-cps-past-the-value-restriction#comment4663665529187287

*)

(* type z
 * type +'n s *)


type z = Zero
type +'a s = Succ

type (-'s, +'t, +'a, -'b, +'z) t =
| Lens : { op : 'r. ('a -> ('b -> 'r) -> 'r) -> 's -> ('t -> 'r) -> 'r } -> ('s, 't, 'a, 'b, ('z * 'z)) t
| Prism : { review : 'b -> 't; preview : 's -> ('a, 't) result } -> ('s, 't, 'a, 'b, ('z s * 'z)) t

type (-'s, +'t, +'a, -'b, 'safety) ty = ('s, 't, 'a, 'b, 'safety) t

type ('s, 'a, 'safety) mono = ('s, 's, 'a, 'a, 'safety) t

let undefined  = assert false

let error_to_none = function
  | Ok o -> Some o
  | Error _ -> None

let lens get set =
  let op k this read = k (get this) (fun b -> read (set this b)) in
  Lens { op }

let prism review preview = Prism { review; preview }

let view : type s t a b n. (s, t, a, b, (n * n)) ty -> s -> a = function
  | Lens { op } -> fun s -> op (fun a _ -> a) s undefined
  | Prism _ -> assert false (* excluded by the occurs check *)

let view_opt (Prism { preview; _}) s = error_to_none (preview s)

let modify (Lens { op }) f s = op (fun a rf -> rf (f a)) s (fun r -> r)

let modify_opt = undefined

let update l b = modify l (fun _ -> b)

let update_opt = undefined

let ( >> ) (Lens { op = f }) (Lens { op = g }) = Lens { op = (fun z -> f (g z)) }

(* Stock lenses *)

let id = Lens { op = (fun _ s rf -> rf s) }

let fst = Lens { op = (fun k (a, x) read -> k a (fun b -> read (b, x))) }

let snd = Lens { op = (fun k (x, b) read -> k b (fun a -> read (x, a))) }

(*  (s = 'a list, t = 'a list, u = 'a, v = 'a)           *)
let head =
  Lens {
    op =
      (fun k list read ->
        let x, xs = (List.hd list, List.tl list) in
        k x (fun b -> read (b :: xs)));
  }

type _ t_list =
  | ( :: ) : ('s, 't, 'a, 'b, 'safety) t * 'l t_list -> (('s, 't, 'a, 'b, 'safety) t * 'l) t_list
  | [] : unit t_list
