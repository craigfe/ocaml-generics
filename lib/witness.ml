type (_, _) eq = Refl : ('a, 'a) eq

type _ equality = ..

module type Inst = sig
  type t

  type _ equality += Eq : t equality
end

type 'a t = (module Inst with type t = 'a)

let make : type a. unit -> a t =
 fun () ->
  let module Inst = struct
    type t = a

    type _ equality += Eq : t equality
  end in
  (module Inst)

let eq : type a b. a t -> b t -> (a, b) eq option =
 fun (module A) (module B) -> match A.Eq with B.Eq -> Some Refl | _ -> None
