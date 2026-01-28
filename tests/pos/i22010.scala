//> using options -source:future

enum Expr1:
  infix case Add1[L, R](l: L, r: R)

import Expr1.Add1
val foo: 1 Add1 2 = ???
