open Generics
open Optics

type my_record = { name : string; flag : bool; count : int }

let (my_record : my_record Type.t), Lens.[ name; flag; count ] =
  let open Type in
  record "my_record" (fun name flag count -> { name; flag; count })
  |+ field "name" string (fun s -> s.name)
  |+ field "flag" bool (fun s -> s.flag)
  |+ field "count" int (fun s -> s.count)
  |> sealr_lens

let record () =
  let r = { name = "foo"; flag = true; count = 2 } in
  r |> Fmt.pr "%a\n" Type.(pp my_record);
  r |> Lens.view name |> Fmt.pr "%a\n" Type.(pp string);
  r |> Lens.view flag |> Fmt.pr "%a\n" Type.(pp bool);
  r |> Lens.view count |> Fmt.pr "%a\n" Type.(pp int)

(* That was fun! Now do it with nested records and combine the lenses ... *)

type nested_record = { a : subrecord_a; b : subrecord_b; c : int }

and subrecord_a = { a_foo : string; a_bar : int }

and subrecord_b = { b_foo : subrecord_a list }

let (subrecord_a : subrecord_a Type.t), Lens.[ a_foo; a_bar ] =
  let open Type in
  record "subrecord_a" (fun a_foo a_bar -> { a_foo; a_bar })
  |+ field "a_foo" string (fun s -> s.a_foo)
  |+ field "a_bar" int (fun s -> s.a_bar)
  |> sealr_lens

let (subrecord_b : subrecord_b Type.t), Lens.[ b_foo ] =
  let open Type in
  record "subrecord_b" (fun b_foo -> { b_foo })
  |+ field "b_foo" (list subrecord_a) (fun s -> s.b_foo)
  |> sealr_lens

let (nested_record : nested_record Type.t), Lens.[ a; b; c ] =
  let open Type in
  record "nested_record" (fun a b c -> { a; b; c })
  |+ field "a" subrecord_a (fun s -> s.a)
  |+ field "b" subrecord_b (fun s -> s.b)
  |+ field "c" int (fun s -> s.c)
  |> sealr_lens

let nested_record () =
  let r =
    { a = { a_foo = "top_level"; a_bar = 1 }; b = { b_foo = [] }; c = 0 }
  in
  r |> Fmt.pr "%a\n" Type.(pp nested_record);
  r |> Lens.(view (a >> a_foo)) |> Fmt.pr "%s\n"

(* r |> Lens.view name |> Fmt.pr "%a\n" Type.(pp string);
 * r |> Lens.view flag |> Fmt.pr "%a\n" Type.(pp bool);
 * r |> Lens.view count |> Fmt.pr "%a\n" Type.(pp int) *)

let () =
  record ();
  nested_record ()
