
//> using options -feature -Werror -preview

import Conversion.into

enum Expr:
  case Neg(e: into[Expr])
  case Add(e1: into[Expr], e2: into[Expr])
  case Const(n: Int)
import Expr.*

given Conversion[Int, Const] = Const(_)

def Test =
  Add(1, Neg(2))
