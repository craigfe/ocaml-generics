## Irmin Store RFC

### Problems with Irmin at present

There are two common issues that arise when using the current Irmin API, both
related to so-called '*path-dependent serialisation*', whereby the location of a
blob inside the tree object uniquely determines the type the user expects of it:

1. __the user knows the high-level type structure of their store.__

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
   the same run-time representation, but with this API they can never share
   blobs in the content-addressable heap.

A better API might have this structure:

```ocaml
let get_spike () = Store.get s (cacti / "dangerous" / "spike")
(* path is typed to return cactuses ∴ no need to untag & assert false*)

let set_camelus c = Store.set ~info s (camels / bactrian / "camelus") c
(* path is typed to set bactrians ∴ no need to tag *)
```

2. __the user knows the low-level type structure of their store.__

```text
**/*.md     # All have type 'markdown'
**/*.txt    # All have type 'text'
```

Again, we can get around this at the application level by tagging our blobs withkkkkkkkkk

Once again, the standard solution to this would be 

These two problems are fundamentally the same

The proposal effectively allows the user to canonify their

## Description of proposed solution

As a simplification, consider sub-trees of an Irmin store as valid Irmin stores
in their own right (this is _almost_ true anyway, barring consideration of the
commit graph which is global). From bottom to top, we have three tiers of API:

 - untyped base level (one type of tree node)
 - typed level (three types of tree node)
 - optical level

Now, at the base level we have this type:

```ocaml
type store =
| Branch of (step * 'a store addr) list
| Blob of bytes

val deref : 'a addr -> 'a
```

where `addr` represents the indirection inherent in the content-addressable
store: this `addr` might be, for example, the hash of a node paired with a
reference to the content-addressable store that contains the node. We can begin
introducing polymorphic parameters in the appropriate places:

```ocaml

```


- abstract `addr : * -> *` type with a supplied type combinator to allow nesting
  stores

We have three different types of tree node at the typed tree representation,
`map`, `sum` and `product`:

- `map` nodes are the same as before (string, 'a store addr) list
- (string, )

We have two different types of trees

- __heterogeneous__ trees with __static__ structure: that is, trees such that we
  know what

If we have the user specify the
  (finite) structure of their store upfront, we can give them getter and setter
  functions (lenses) for each level of the tree that will behave in a type-safe
  way.
  
- __homogeneous__ trees with __dynamic__ structure (this is really a subset
  of the above, where the 'structure' in question is the classic recursive tree
  datatype)

The solution is to allow these to be nested inside each other arbitrarily.
Nesting a homogeneous tree inside a heterogeneous one solves problem (A) given
above.


Specifically, we need to do the following:

 - extend the type combinators in 
 
## Conclusion

The two types specified above.

## Limitations

### unable to precisely determine when access is partial

This API introduces something new: certain accesses of an Irmin store may be
__guaranteed__ to succeed, since the path from root to blob goes via no sum
types.

We'd love to be able to determine when the composition of multiple lenses is
partial, so that the resulting `get` operation can return an `option` _only_
when this is strictly necessary (i.e. the access goes via a sum type, either
explicitly defined by the user heterogeneously).
