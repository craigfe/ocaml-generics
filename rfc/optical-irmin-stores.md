# RFC - Heterogeneous Irmin stores via Effectful Optics

## Problems with Irmin at present

Irmin is parameterised over a user-supplied data type to be used for
representing leaves (blobs) of the store. The blob type `b` must be passed with:

- a corresponding generic `b Type.t`, defined by hand via the `Irmin.Type`
  combinators or via `ppx_irmin`, to allow Irmin to derive its own
  pretty-printing, hashing and serialisation functions for `b`.
- a merge combinator `b option Merge.t` used to resolve blob-level conflicts.

With this API, all blobs in the store must have the same OCaml representation.
This leads to unpleasant interactions when attempting to store different types
of data in different regions of the store. In particular, we have observed two
different issues:

1. **the user knows the high-level type structure of their store.**

Application constraints quite often lead to certain regions of the Irmin store
having pre-determined type:

```text
camels/dromedary/**  # All blobs have type 'dromedary'
camels/bacrian/**    # All blobs have type 'bactrian'
cacti/**             # All blobs have type 'cactus'
```

With the current API, the only thing to be done is to make a tagged union and
`assert false` in the appropriate places:

```ocaml
type blob = Dromedary of dromedary | Bactrian of bactrian | Cactus of cactus
[@@deriving irmin]

let get_spike () =
  Store.get s [ "cacti"; "dangerous"; "spike" ] >|= function
  | Cactus c -> c
  | _ -> assert false (* never occurs in a well-formed application *)

let set_camelus c =
  Store.set ~info s [ "camels"; "bactrian"; "camelus" ] (Bactrian c)
  (* must tag with `Bactrian` and promise never to give the wrong tag *)
```

This works, but has three issues

- heavy-weight syntax;
- forces the user to maintain their own type-safety;
- requires serialising tags in the content-addressable heap, losing
  opportunities for sharing. For example, `Dromedary` and `Bactrian` may have
  the same run-time representation, but with this API they can never share blobs
  in the content-addressable heap.

2. **the user knows the low-level type structure of their store.**

The type of a blob may also be determined by a _suffix_ of the path used to
access it, for example:

```text
**/*.md     # All have type 'markdown'
**/*.txt    # All have type 'text'
```

Again, we can get around this at the application level by using a sum type for
the blobs:

```ocaml
type file = Markdown of markdown | Text of text [@@deriving irmin]
(* where `markdown` and `text` have appropriate serialisers defined *)

let get_markdown name =
  Store.get s [ "files"; (name ^ ".md") ] >|= function
  | Markdown m -> m
  | _ -> assert false
```

but the three above are still prevalent. In this particular example, we might
particularly care about introducing serialised tags, since it would cause the
`.md` files to no longer be parse-able as Markdown files when using a
file-system backend for Irmin.

## Proposed solution

### Optical get/set API

A better API to solve problem #1 might look like the following:

```ocaml
let get_spike () = Store.get s (cacti / steps ["dangerous"; "spike"])
(* path is typed to return cactuses ∴ no need to untag & assert false*)

let set_camelus c = Store.set ~info s (camels / bactrian / step "camelus") c
(* path is typed to set bactrians ∴ no need to tag *)
```

where `cacti`, `camels`, `bactrian` are some form of 'first-class projection'
that can be used to get/set a sub-component of a complex type. Such objects are
typically referred to as [optics][optics]. In this case, the 'complex' type is
store as a product type defined by:

```ocaml
type camel_store = {
  dromedary: dromedary tree;
  bactrian: bactrian tree;
  cacti: cactus tree
}

type desert_store = {
  camels: camel_store
  cacti: cactus tree
}
```

where `'b tree` is a standard Irmin tree object with a single blob type `'b` and
indexed by a sequence of 'steps' (optics over product types are known as
'_lenses_').

Similarly, we can imagine a better API for the file-system case:

```ocaml
let get_markdown name = Store.get s (step "files" / name / md)
```

This is effectively the same as what we were doing before, with two differences:

- the sum type has been reified into the Irmin store (meaning that the
  serialised blob need not contain a tag);

- the assertion of the file's type is combined with the assertion that it exists
  at the location `files/<name>.md` (saving some boilerplate code). In this
  case, the `md` value is a 'prism' (optic for sum types) that provides a sum
  projection (i.e. `file -> markdown option`).

### Types of tree node

To generalise from these examples, the proposal is to generalise from having the
user define 'blob' types to defining the type structure of the entire store. We
will have three types of tree object, which (might) be nested arbitrarily inside
each other:

- **'primitive' nodes** (name tbd), which provide the old behaviour of a
  homogeneous tree with unbounded children per branch
  (`'a tree -> string -> 'a tree`). This will be the underlying representation
  for every store (glossing over some details about serialising tags for
  non-suffix sum types). To the user, it appears like an abstract `tree` product
  type with a pre-defined type combinator and lens constructor:

```ocaml
val tree : 'a Irmin.Type.t -> 'a tree Irmin.Type.t

(* blob projections from a sequence of steps *)
val steps : string list -> ('a heterogeneous_store, 'a) lens
```

- **product nodes**, defining a fixed number of components (via
  `Irmin.Type.record`) and accessed via lenses.

- **sum nodes**, defining a fixed number of cases (via `Irmin.Type.variant`) and
  accessed via prisms.

This has the advantage of easily reproducing the old behaviour as `b tree` while
also allowing arbitrary algebraic type structure to be encoded into the Irmin
store via generics. The old API can be preserved as a simplified `Contents`
functor that uses `b tree` as the store type internally and hides all uses of
optics.

### Generic construction of optics

It is possible to derive the necessary lenses and prisms with generics, avoiding
introducing any additional boilerplate:

```ocaml
type +'a addr

val unreturn : 'a t -> 'a
val unbind : 'a -> ('a t -> 'b) -> 'b

```

```ocaml
type my_record = { name : string; flag : bool; count : int }

let my_record, Lens.[ name; flag; count ] =  (* get the generic and the lenses *)
  let open Type in
  record "my_record" (fun name flag count -> { name; flag; count })
  |+ field "name" string (fun s -> s.name)
  |+ field "flag" bool (fun s -> s.flag)
  |+ field "count" int (fun s -> s.count)
  |> sealr_lens                              (* seal with lens *)
```

Another (more extensive) option might be to provide optic construction for
arbitrary generics.

### Monad-parameterised optics

A key advantage of using optics as an Irmin interface is to represent the
indirection of the content-addressable store: accessing/updating the field of a
'store'-d record requires access to a content-addressable heap that may have its
own effect monad. We can provide an optics library that is parameterised over
these effects, and then specialise them for Irmin:

```ocaml
module Lens_with_effect (Effect : sig
  type addr (* Reference / hash in the content-addressable heap *)

  (* Monadic effects in the heap *)
  type +'a io
  val return : 'a -> 'a io
  val bind : 'a io -> ('a -> 'b io) -> 'b io

  val deref : 'a addr -> 'a io
  val update : 'a addr -> ('a -> 'b) -> 'b addr io
end) =
struct
  open Effect

  type (-'s, +'t, +'a, -'b) lens

  (* Composition with indirection *)
  val ( / ) :
    ('a, 'b, 'c addr, 'd addr) lens ->
    ('c, 'd, 'e addr, 'f addr) lens ->
    ('a, 'b, 'e addr, 'f addr) lens

  (* Get/set with effect and indirection *)
  val view : ('s, 't, 'a addr, 'b addr) t -> 's -> 'a
  val modify : ('s, 't, 'a addr, 'b addr) t -> ('a -> 'b) -> 's -> 't io
end
```

There are many such constructions that achieve the same expressivity: if anyone
has an idea of a principled way of selecting one, please let me know
:slightly_smiling_face:

### Backend-imposed restrictions

Certain backends might want to impose restrictions / conventions about the type
structure contained in the store in ways that make sense for that specific data
representation. For example, the Git-compatible backends could restrict sum
types to a dedicated '_extension_' lens that may only be applied at the end of
the path.

The solution should be functorised in such a way as to allow for this use-case.

## Advantages

- **user-defined types with shared subcomponents**. This solution provides a
  feature that has been under discussion for inclusion in Irmin
  (https://github.com/mirage/irmin/issues/478): the ability to define
  'structured' blob types (where internal components can be shared across the
  content-addressable heap).

## Limitations

- **runtime overhead due to generics**. Extracting a blob from a tree object
  will now require generics, making it slower (except in the case where only
  primitive nodes are used). As always with generics, we might expect
  significant performance improvements with the use of multi-stage programming.

- **unable to precisely determine when access is partial**. This API introduces
  something new: certain accesses of an Irmin store may be **guaranteed** to
  succeed, since the path from root to blob goes via no sum types. The lens API
  has no way to track the totality of accesses (without more powerful type-level
  programming features made possible by https://github.com/ocaml/RFCs/pull/5).

[optics]: https://hackage.haskell.org/package/optics-0.1/docs/Optics.html
