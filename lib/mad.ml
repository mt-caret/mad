open Core_kernel

module Vec = Vec

module Node = struct
  type t
    = Nullary
    | Unary of Float.t * Int.t
    | Binary of Float.t * Int.t * Float.t * Int.t
    [@@deriving sexp]
end

module Var = struct
  type t =
    { tape : Node.t Vec.t
    ; value: Float.t 
    ; index: Int.t
    } [@@deriving sexp]

  let push_node tape node =
    let len = Vec.length tape in
    Vec.push tape node;
    len
  ;;

  let create tape value =
    { tape
    ; value
    ; index = push_node tape Nullary
    }
  ;;

  let sin { tape; value; index; } =
    { tape
    ; value = Float.sin value
    ; index =
        Unary (Float.cos value, index)
        |> push_node tape
    }
  ;;

  let cos { tape; value; index; } =
    { tape
    ; value = Float.cos value
    ; index =
        Unary (-. Float.sin value, index)
          |> push_node tape 
    }
  ;;

  let add (a : t) (b : t) =
    { tape = a.tape
    ; value = a.value +. b.value
    ; index =
        Binary (1., a.index, 1., b.index)
          |> push_node a.tape
    }
  ;;

  let mult (a : t) (b : t) =
    { tape = a.tape
    ; value = a.value *. b.value
    ; index =
        Binary (b.value, a.index, a.value, b.index)
          |> push_node a.tape
    }
  ;;

  let grad { tape; value = _; index } =
    let len = Ref.create (Vec.length tape) in
    let grads = Array.create ~len:!len 0. in
    printf "%d, %d" !len index;
    grads.(index) <- 1.;
    len := !len - 1;
    while !len >= 0 do
      let deriv = grads.(!len) in
      (match Vec.get tape !len with 
      | Nullary -> ()
      | Unary (w, i) ->
        grads.(i) <- grads.(i) +. w *. deriv
      | Binary (w1, i1, w2, i2) ->
        grads.(i1) <- grads.(i1) +. w1 *. deriv;
        grads.(i2) <- grads.(i2) +. w2 *. deriv);
      len := !len - 1
    done;
    grads
  ;;
end