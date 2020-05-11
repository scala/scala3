import scala.quoted._

inline def mcr(body: => Any): Unit = ${mcrImpl('body)}

def mcrImpl[T](using s: Scope)(body: s.Expr[Any]): s.Expr[Any] = {
  import s.tasty._

  val bTree = body
  val under = bTree.underlyingArgument

  val res = '{Box(${under.asInstanceOf[Term].seal})}
  res
}

class Box(inner: => Any)
