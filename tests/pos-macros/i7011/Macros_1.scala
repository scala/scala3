import scala.quoted._

inline def mcr(body: => Any): Unit = ${mcrImpl('body)}

def mcrImpl[T](body: Expr[Any])(using Quotes) : Expr[Any] = {
  import quotes.reflect._

  val bTree = Term.of(body)
  val under = bTree.underlyingArgument

  val res = '{Box(${under.asInstanceOf[Term].asExpr})}
  res
}

class Box(inner: => Any)
