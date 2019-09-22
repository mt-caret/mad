open Core_kernel
open Mad

let () =
  let tape = Vec.create () in
  let x = Var.create tape 0.5 in 
  let y = Var.create tape 4.2 in 
  let z = Var.add (Var.mult x y) (Var.sin x) in
  let grad = Var.grad z in
  printf "z = %f, dz/dx = %f, dz/dy = %f" z.value grad.(x.index) grad.(y.index)