# Generics library in OCaml

Work-in-progress extensions of the [Irmin.Type][irmin-type] generics library.

## TODO:

- abstract `addr : * -> *` type with a supplied type combinator to allow nesting
  stores. Should be able to 'count' the holes introduced by the 'addr'
  combinator, so that the structure of the node can be deteremined (essentially,
  we need to map it onto a (data, children list) pair -- or perhaps we
  should/must disallow data? recordifying)

- OR: lift type combinators to allow injection of the `addr` modality and
  require this injection

- Kleisli composition for `(*, * addr) Optic.mono`: 
```ocaml
val (//) : ('a, 'b addr) Optic.mono -> ('b, 'c addr) Optic.mono -> ('a, 'c addr) Optic.mono
```

- modify generic type combinators to return optics
  - record combinator returns a lens for each field (and works with setters)
  - variant combinator returns a prism for each field

[irmin-type]: https://github.com/mirage/irmin/blob/master/src/irmin/type.mli
