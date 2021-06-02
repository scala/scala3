trait Expr[T]
final class Inv[T] extends Expr[T]

def foo[X, T1 >: X <: X, T2](m: Expr[T2]): T2 = m match {
  case _: Inv[T1] =>
    (??? : X)
}

