mad

a reverse-mode automatic differentiation toy implementation.

straightforward port from
<https://rufflewind.com/2016-12-30/reverse-mode-automatic-differentiation>

```
(() 0)
((Nullary) 1)
((Nullary Nullary) 2)
((Nullary Nullary (Unary 0.87758256189037276 0) (Binary 4.2 0 0.5 1)
  (Binary 1 3 1 2) Nullary (Unary 0.87758256189037276 0)
  (Binary 4.2 0 0.5 1))
 5)
z = 2.579426, dz/dx = 5.077583, dz/dy = 0.500000
```
