import scala.quoted._

trait IsExpr[T] {
  type Underlying
  def toExpr(x: T): Expr[Underlying]
}

given [U] as IsExpr[Expr[U]] = new IsExpr[Expr[U]] {
  type Underlying = U
  def toExpr(x: Expr[U]): Expr[U] = x
}

def f(x: Any): String = x.toString

def g[T](x: T) given (e: IsExpr[T], tu: Type[e.Underlying]): given QuoteContext => Expr[String] = {
  val underlying: Expr[e.Underlying] = e.toExpr(x)
  '{f($underlying)}
}

inline def mcr(): Any = ${mcrImpl}
def mcrImpl given QuoteContext: Expr[Any] = {
  val x = '{1}
  g(x)
}
