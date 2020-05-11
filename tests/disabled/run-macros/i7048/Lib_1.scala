import scala.quoted._

trait IsExpr[T] {
  val s: Scope
  type Underlying
  def toExpr(x: T): s.Expr[Underlying]
}

given [U](using s0: Scope) as IsExpr[s0.Expr[U]] = new IsExpr[s0.Expr[U]] {
  val s = s0
  type Underlying = U
  def toExpr(x: Expr[U]): Expr[U] = x
}

def f(x: Any): String = x.toString

def g[T](x: T)(using s0: Scope)(using e: IsExpr[T] { val s: s0 })(using tu: s0.Type[e.Underlying]): s0.Expr[String] = {
  val underlying: s.Expr[e.Underlying] = e.toExpr(x)
  '{f($underlying)}
}

inline def mcr(): Any = ${mcrImpl}
def mcrImpl(using s: Scope): s.Expr[Any] = {
  val x = '{1}
  g(x)
}
