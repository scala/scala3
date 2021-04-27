import scala.quoted.*

trait IsExpr[T] {
  type Underlying
  def toExpr(x: T): Expr[Underlying]
}

given [U]: IsExpr[Expr[U]] = new IsExpr[Expr[U]] {
  type Underlying = U
  def toExpr(x: Expr[U]): Expr[U] = x
}

def f(x: Any): String = x.toString

def g[T](x: T)(using e: IsExpr[T])(using tu: Type[e.Underlying]): Quotes ?=> Expr[String] = {
  val underlying: Expr[e.Underlying] = e.toExpr(x)
  '{f($underlying)}
}

inline def mcr(): Any = ${mcrImpl}
def mcrImpl(using Quotes): Expr[Any] = {
  val x = '{1}
  g(x)
}
