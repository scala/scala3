trait Expr { type T }
case class Inv[X](x: X) extends Expr { type T = X }
case class Inv2[X](x: X) extends Expr { type T >: X }

def eval(e: Expr): e.T = e match
  case Inv(x) => x
  case Inv2(x) => x

