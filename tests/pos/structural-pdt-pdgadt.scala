type typed[E <: Expr, V] = E & { type T = V }

trait Expr { type T }
case class LitInt(x: Int) extends Expr { type T >: Int }
case class Add(e1: Expr typed Int, e2: Expr typed Int) extends Expr { type T >: Int }
case class LitBool(x: Boolean) extends Expr { type T >: Boolean }
case class Or(e1: Expr typed Boolean, e2: Expr typed Boolean) extends Expr { type T >: Boolean }

def eval(e: Expr): e.T = e match
  case LitInt(x) => x
  case Add(e1, e2) => eval(e1) + eval(e2)
  case LitBool(b) => b
  case Or(e1, e2) => eval(e1) || eval(e2)
