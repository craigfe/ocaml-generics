let undefined _ = assert false

type ('k, 'v) map = ('k, 'v) Hashtbl.t

type ('a, 'b) bijection = { inj : 'a -> 'b; surj : 'b -> 'a }

(** Capability-indexed heap effects *)
module type IO = sig
  type (+'a, 'cap) t

  val return : 'a -> ('a, 'cap) t

  val bind : ('a, 'cap) t -> ('a -> ('b, 'cap) t) -> ('b, 'cap) t
end

module Identity = struct
  type (+'a, 'cap) t = 'a

  let return x = x

  let bind x f = f x
end

(** Content-addressable heap *)

module Heap = struct
  module type S = sig
    type ('hash, 'elt) t

    type (+'a, 'cap) io

    val v : ('elt -> 'hash) -> ('hash, 'elt) t

    val get : ('hash, 'elt) t -> 'hash -> ('elt, [ `Read ]) io

    val add : ('hash, 'elt) t -> 'elt -> ('hash, [ `Write ]) io
  end

  module Make (IO : IO) : S with type (+'a, 'cap) io := ('a, 'cap) IO.t = struct
    type ('hash, 'elt) t = {
      hashfn : 'elt -> 'hash;
      tbl : ('hash, 'elt) Hashtbl.t;
    }

    let v hashfn = { hashfn; tbl = Hashtbl.create 0 }

    let get { tbl; _ } a = Hashtbl.find tbl a |> IO.return

    let add { tbl; hashfn } e =
      let addr = hashfn e in
      Hashtbl.add tbl addr e;
      IO.return addr
  end
end

(* module Heap : sig
 *   type ('tree, 'blob) t
 * 
 *   type ('tree, 'blob, 'a) addr
 * 
 *   val get : ('tree, 'blob) t -> ('tree, 'blob, 'a) addr -> 'a
 * 
 *   val mk_blob_addr : ('tree, 'blob) t -> 'blob -> ('tree, 'blob, 'blob) addr
 * 
 *   val mk_tree_addr : ('tree, 'blob) t -> 'tree -> ('tree, 'blob, 'tree) addr
 * end = struct
 *   type hash = string
 * 
 *   type ('tree, 'blob, _) addr =
 *     | Tree : hash -> ('tree, 'blob, 'tree) addr
 *     | Blob : hash -> ('tree, 'blob, 'blob) addr
 * 
 *   type ('tree, 'blob) t = {
 *     tree_hash : 'tree -> hash;
 *     blob_hash : 'blob -> hash;
 *     tree_heap : (hash, 'tree) Hashtbl.t;
 *     blob_heap : (hash, 'blob) Hashtbl.t;
 *   }
 * 
 *   let get : type a tree blob. (tree, blob) t -> (tree, blob, a) addr -> a =
 *    fun { tree_heap; blob_heap; _ } -> function
 *     | Tree t -> Hashtbl.find tree_heap t
 *     | Blob b -> Hashtbl.find blob_heap b
 * 
 *   let mk_blob_addr { blob_hash; _ } b = Blob (blob_hash b)
 * 
 *   let mk_tree_addr { tree_hash; _ } t = Tree (tree_hash t)
 * end *)

module Homo_store : sig
  (* content addressable store -- hashing a value gives us access to it's address in the store *)

  (* Concrete API *)
  type ('value, 'step, 'hash, 'blob) meta

  type ('value, 'step, 'hash, 'blob) t =
    ('value, 'step, 'hash, 'blob) meta * ('value, 'step, 'hash, 'blob) node

  and ('value, 'step, 'hash, 'blob) node =
    | Tree of ('step, 'hash) map
    | Blob of 'value

  val v :
    (('value, 'step, 'hash, 'blob) node -> 'hash) ->
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
  module Heap = Heap.Make (Identity)

  type ('value, 'step, 'hash, 'blob) t =
    ('value, 'step, 'hash, 'blob) meta * ('value, 'step, 'hash, 'blob) node

  and ('value, 'step, 'hash, 'blob) node =
    | Tree of ('step, 'hash) map
    | Blob of 'value

  and ('value, 'step, 'hash, 'blob) meta = {
    hashfn : ('value, 'step, 'hash, 'blob) node -> 'hash;
    serialise : ('value, 'blob) bijection;
    heap : ('hash, ('value, 'step, 'hash, 'blob) node) Heap.t ref; (* TODO: remove ref *)
  }

  let v_node () = Hashtbl.create 0

  let v hashfn serialise =
    let heap = ref (Heap.v hashfn) in
    let node = v_node () in
    ({ hashfn; serialise; heap }, Tree node)

  let rec get : 'step list -> ('value, 'step, _, _) t -> 'value option =
   fun steps (meta, t) ->
    match (steps, t) with
    | [], Tree _ -> None
    | _ :: _, Blob _ -> None
    | [], Blob b -> Some b
    | step :: steps, Tree children ->
        let next = step |> Hashtbl.find children |> Heap.get !(meta.heap) in
        get steps (meta, next)

  let set :
      type value step hash blob.
      step list ->
      value ->
      (value, step, hash, blob) t ->
      (value, step, hash, blob) t =
   fun s v (meta, node) ->
    (* Inner recursive function propagates up the hash of the value, so that we
       can add it to the parent *)
    let rec aux :
        step list ->
        (value, step, hash, blob) node ->
        (value, step, hash, blob) node * hash =
     fun steps node ->
      match (steps, node) with
      | [], _ ->
          let blob_node = Blob v in
          let hash = Heap.add !(meta.heap) blob_node in
          (blob_node, hash)
      | s :: ss, Tree children ->
          let child_hash = Hashtbl.find children s in
          let child = Heap.get !(meta.heap) child_hash in
          let _, child_hash = aux ss child in

          (* Update to the new child *)
          Hashtbl.replace children s child_hash;

          (* Re-hash the node *)
          (* Put the new tree object in the heap *)
          let node = Tree children in
          let hash = Heap.add !(meta.heap) node in

          (node, hash)
      | (_ :: _ as steps), Blob _ ->
          (* Replace the existing blob with new tree *)
          aux steps (Tree (v_node ()))
    in
    (meta, fst (aux s node))
end

(* module Hetero_store : sig
 *   type 'typ t
 * 
 *   type +'v addr
 * 
 *   val get : ('typ, 'v addr) Optics.Lens.mono -> 'typ t -> 'v option io
 * end = struct
 *   type 'typ t
 * 
 *   type addr
 * 
 *   let get = undefined
 * end *)

type hetero_store = { name : string; tmp : bytes; context : int }

let _either : hetero_store Type.t =
  let open Type in
  record "either" (fun name tmp context -> { name; tmp; context })
  |+ field "name" string (fun s -> s.name)
  |+ field "tmp" bytes (fun s -> s.tmp)
  |+ field "context" int (fun s -> s.context)
  |> sealr
