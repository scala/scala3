type Const = [X] =>> Int

trait Expr[-F[_]]
case class ConstExpr() extends Expr[Const]

def foo[F[_], A](e: Expr[F]) = e match
  case _: ConstExpr =>
    val i: Int = ??? : F[A]
