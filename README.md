# Generics library in OCaml

Mostly a copy of functionality available in the [Irmin.Type][irmin-type]
generics library, but without the dependencies on e.g. hashing and binary
encoding that are necessary there.

[irmin-type]: https://github.com/mirage/irmin/blob/master/src/irmin/type.mli

## RFC

- modifies the Irmin store so that subtrees are valid stores

## TODO:

- abstract `addr : * -> *` type with a supplied type combinator to allow nesting stores
- Kleisli composition for `(*, * addr) Optic.mono`: 
```ocaml
val (//) : ('a, 'b addr) Optic.mono -> ('b, 'c addr) Optic.mono -> ('a, 'c addr) Optic.mono
```
- modify generic type combinators to return optics
  - record combinator returns a lens for each field (and works with setters)
  - variant combinator returns a prism for each field
