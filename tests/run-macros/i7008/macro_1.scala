import scala.quoted._

case class Box[T](v: T)

inline def mcr(expr: => Boolean): Unit = ${mcrProxy('expr)}

def mcrProxy(using s: Scope)(expr: s.Expr[Boolean]): s.Expr[Unit] = {
  val res = mcrImpl[Boolean]('{ (esx: Seq[Box[Boolean]]) => () }, expr)
  // println(s"Out: ${res.show}")
  res
}

def mcrImpl[T](using s: Scope)(func: s.Expr[Seq[Box[T]] => Unit], expr: s.Expr[T])(using tt: s.Type[T]): s.Expr[Unit] = {
  val arg = Varargs(Seq('{(Box($expr))}))
  Expr.betaReduce('{$func($arg)})
}