open Core_kernel
open Mad

let print_tape tape =
  [%sexp_of: Node.t Vec.t] tape
  |> print_s
;;

let () =
  let tape = Vec.create () in
  print_tape tape;
  let x = Var.create tape 0.5 in 
  print_tape tape;
  let y = Var.create tape 4.2 in 
  print_tape tape;
  let z = Var.add (Var.mult x y) (Var.sin x) in
  print_tape tape;
  let grad = Var.grad z in
  printf "z = %f, dz/dx = %f, dz/dy = %f\n" z.value grad.(x.index) grad.(y.index)