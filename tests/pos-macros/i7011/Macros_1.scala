import scala.quoted._

inline def mcr(body: => Any): Unit = ${mcrImpl('body)}

def mcrImpl[T](body: Expr[Any])(using QuoteContext) : Expr[Any] = {
  import qctx.reflect._

  val bTree = body.unseal
  val under = bTree.underlyingArgument

  val res = '{Box(${under.asInstanceOf[Term].asExpr})}
  res
}

class Box(inner: => Any)
