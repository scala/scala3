trait Expr[T]
final class Lit[T] extends Expr[T]

def foo[X, T1 >: X, T2](m: Expr[T2]): T2 = m match {
  case _: Lit[T1] => ??? : X
}
