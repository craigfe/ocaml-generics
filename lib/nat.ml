
(* Any free type variables which occur only in covariant positions outside of
reference types can be safely generalised, even for termsthat are not syntactic
values (Garrigue, FLOPS 2004). *)

(* Avoid the value restriction by forcing the types to be covariant*)
type z
type +'n s
type +'i t = int

let zero = 0
let succ = (+) 1
let add = (+)
let to_int n = n
