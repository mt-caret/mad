open Core_kernel

type 'a t = 'a Array.t Ref.t * Int.t Ref.t [@@deriving sexp]

let get (t, l) i =
  if i >= !l
  then raise (Invalid_argument "index out of bounds")
  else !t.(i)
;;

let equal (t1 , l1) (t2, l2) ~equal =
  if !l1 <> !l2 then false else
  let rec go i accum = 
    if i >= !l1 then accum else
    go (i + 1) (accum && equal (get (t1, l1) i) (get (t2, l2) i))
  in go 0 true
;;

let set (t, l) i x =
  if i >= !l
  then raise (Invalid_argument "index out of bounds")
  else !t.(i) <- x
;; 

let push (t, l) x =
  if !l = 0 then
    t := Array.create ~len:1 x
  else begin
    if !l = (Array.length !t) then
      t := Array.append !t !t;
    !t.(!l) <- x;
    l := !l + 1
  end
;;

let create () =
  ( Ref.create (Array.create ~len:0 (Obj.magic 0))
  , Ref.create 0
  )
;;

let length (_, l) = !l

let copy (t, l) =
  ( Ref.create (Array.copy !t)
  , Ref.create (!l)
  )

module G = Quickcheck.Generator
let rec gen g =
  let open G.Let_syntax in
  match%bind G.size with
  | 0 -> G.return (create ())
  | n ->
    let%map t = G.with_size ~size:(n - 1) (gen g)
    and e = g
    in
    push t e;
    t
;;

(* TODO: implement later? *)
let obs _ =
  Quickcheck.Observer.singleton ()
;;

module S = Quickcheck.Shrinker
let shrinker _ =
  S.create (fun vec ->
    let (t, l) = copy vec in
    l := !l - 1;
    Sequence.singleton (t, l)
  )

let run_test f = Quickcheck.test (gen Int.gen) ~f

let prop_should_equal_itself (t : Int.t t) =
  assert (equal ~equal:Int.equal t t)
;;

let prop_should_equal_copy (t : Int.t t) =
  assert (equal ~equal:Int.equal t (copy t))
;;

let prop_set_set_is_equal_to_set ((t, i): Int.t t * Int.t) =
  if 0 <= i && i < length t then begin
    let t_copy = copy t in
    set t_copy i 0;
    set t_copy i 0;
    set t i 0;
    assert (equal ~equal:Int.equal t t_copy)
  end
;;

let%test_unit _ = run_test prop_should_equal_itself
let%test_unit _ = run_test prop_should_equal_copy

let%test_unit _ =
  Quickcheck.test (G.tuple2 (gen Int.gen) Int.gen)
    ~f:prop_set_set_is_equal_to_set
;;