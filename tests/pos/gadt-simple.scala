trait Expr[+T]
final class IntVal extends Expr[Int]

def eval[T](x: Expr[T]): T = x match {
  case _: IntVal => 0
}

// val unsound = new IntVal with Expr[Boolean]

// val x: Boolean = eval[Boolean](unsound)
