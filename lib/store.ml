(* Type level Booleans *)
let undefined _ = assert false

type ttrue = |

type tfalse = |

type ('k, 'v) map = ('k, 'v) Hashtbl.t

type ('k, 'v) heap = ('k, 'v) Hashtbl.t

type ('a, 'b) bijection = { inj : 'a -> 'b; surj : 'b -> 'a }

(** Content-addressable heap *)
(* module Heap : sig
 *   type 'a t
 * 
 *   type 'a addr
 * 
 *   val get : 'a t -> 'a addr -> 'a
 * 
 *   val add : 'a t -> 'a -> 'a t
 * 
 *   val hash : 'a -> 'a addr
 * 
 *   val v : ('a -> string) -> 'a t
 * 
 * end = struct
 *   module SMap = Map.Make (String)
 * 
 *   type 'a addr = string
 * 
 *   type 'a t = ('a -> string) * 'a SMap.t
 * 
 *   let get = Hashtbl.find
 * 
 * let add = Hashtbl.add
 * 
 *   let v f = (f, Hashtbl.create 0)
 * end *)


module Homo_store : sig
  (* content addressable store -- hashing a value gives us access to it's address in the store *)

  (* Concrete API *)
  type ('value, 'step, 'hash, 'blob) meta

  type ('value, 'step, 'hash, 'blob) t =
    ('value, 'step, 'hash, 'blob) meta * ('value, 'step, 'hash, 'blob) node

  and ('value, 'step, 'hash, 'blob) node =
    | Tree of ('step, 'hash) map
    | Blob of 'blob

  val v :
    ('value -> 'hash) ->
    ('value, 'blob) bijection ->
    ('value, 'step, 'hash, 'blob) t

  (** Abstract API using lenses *)

  (** Abstract get/set API *)

  val get : 'step list -> ('value, 'step, _, _) t -> 'value option
  (* optional because the path may not exist, or may end in a sub-tree *)

  val set :
    'step list ->
    'value ->
    ('value, 'step, 'hash, 'blob) t ->
    ('value, 'step, 'hash, 'blob) t

end = struct
  type ('value, 'step, 'hash, 'blob) t =
    ('value, 'step, 'hash, 'blob) meta * ('value, 'step, 'hash, 'blob) node

  and ('value, 'step, 'hash, 'blob) node =
    | Tree of ('step, 'hash) map
    | Blob of 'blob

  and ('value, 'step, 'hash, 'blob) meta = {
      hashfn : 'value -> 'hash;
      serialise : ('value, 'blob) bijection;
      heap : ('hash, ('value, 'step, 'hash, 'blob) t) map ref;
      (* This heap is mutable, when we actually want an immutable one...
         must be a ref because we want substores to be valid stores, but they
         share the same heap. *)
    }

  let v_node () = Hashtbl.create 0

  let v_heap () = ref (Hashtbl.create 0)

  let v hashfn serialise =
    let heap = v_heap () in
    let node = v_node () in
    ({ hashfn; serialise; heap }, Tree node)

  let rec get steps ({ heap; serialise; _ }, t) =
    match (steps, t) with
    | [], Blob b -> Some (serialise.surj b)
    | [], Tree _ -> None
    | _ :: _, Blob _ -> None
    | step :: steps, Tree(children) ->
        step
        |> Hashtbl.find children
        |> Hashtbl.find (!heap)
        |> get steps

  let rec set steps ({ heap; serialise; hashfn }, t) =
    match (steps, t) with
    | ([], _) -> undefined ()
    | _ -> undefined ()

end
