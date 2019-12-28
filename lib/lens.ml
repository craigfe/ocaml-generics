(* van Laarhoven encoding of lenses, based on CPS *)
type (-'s, +'t, +'a, -'b) t = {
  op : 'r. ('a -> ('b -> 'r) -> 'r) -> 's -> ('t -> 'r) -> 'r;
}

let undefined _ = assert false

let lens get set =
  let op k this read = k (get this) (fun b -> read (set this b)) in
  { op }

let view { op } s = op (fun a _ -> a) s undefined

let modify { op } f s = op (fun a rf -> rf (f a)) s (fun r -> r)

let update l b = modify l (fun _ -> b)

let ( >> ) = undefined

(* Generic *)

type 'a homo_store = unit

let hs : 'a Type.t -> 'a homo_store Type.t = undefined

type hetero_store = {
  name : string homo_store;
  tmp : bytes homo_store;
  context : int homo_store;
}

let _either : hetero_store Type.t =
  let open Type in
  record "either" (fun name tmp context -> { name; tmp; context })
  |+ field "name" (hs string) (fun s -> s.name)
  |+ field "tmp" (hs bytes) (fun s -> s.tmp)
  |+ field "context" (hs int) (fun s -> s.context)
  |> sealr

(* let left : (either_store, string, string, either_store) t =
 *   lens (fun (`Left l, _) -> l) (fun s b -> {s with left = b}) *)
