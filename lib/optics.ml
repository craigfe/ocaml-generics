let undefined _ =
  let exception Undefined in
  raise Undefined

module Lens = struct
  type (-'s, +'t, +'a, -'b) t = { op : 'r. ('a -> ('b -> 'r) -> 'r) -> 's -> ('t -> 'r) -> 'r }
(** We use an optic representation very similar to the standard 'van Laarhoven'
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

@see https://stackoverflow.com/questions/29187287/sneaking-lenses-and-cps-past-the-value-restriction#comment4663665529187287
*)

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  let v get set =
    let op k this read = k (get this) (fun b -> read (set this b)) in
    { op }

  let view { op } s = op (fun a _ -> a) s undefined

  let modify { op } f s = op (fun a rf -> rf (f a)) s (fun r -> r)

  let update l b = modify l (fun _ -> b)

  let ( >> ) { op = f } { op = g } = { op = (fun z -> f (g z)) }

  (* Provided lenses *)
  let id = { op = (fun _ s rf -> rf s) }

  let fst = { op = (fun k (a, x) read -> k a (fun b -> read (b, x))) }

  let snd = { op = (fun k (x, b) read -> k b (fun a -> read (x, a))) }

  let head =
    {
      op =
        (fun k list read ->
          let x, xs = (List.hd list, List.tl list) in
          k x (fun b -> read (b :: xs)));
    }

  type _ t_list =
    | ( :: ) : ('s, 't, 'a, 'b) t * 'l t_list -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
end

module Prism = struct
  type (-'s, +'t, +'a, -'b) t = { review : 'b -> 't; preview : 's -> ('a, 't) result }

  type ('s, 'a) mono = ('s, 's, 'a, 'a) t

  let v review preview = { review; preview }

  let ( >> ) = undefined

(* f g = {
 *     review = (fun x -> f.review (g.review x));
 *     preview = (fun s -> g.preview (f.preview s)); *)
  (* } *)

  (* Provided prisms *)

  type _ t_list =
    | ( :: ) : ('s, 't, 'a, 'b) t * 'l t_list -> (('s, 't, 'a, 'b) t * 'l) t_list
    | [] : unit t_list
end
