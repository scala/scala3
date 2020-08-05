import scala.quoted._

inline def mcr(body: => Any): Unit = ${mcrImpl('body)}

def mcrImpl[T](body: Expr[Any])(using ctx: QuoteContext) : Expr[Any] = {
  import ctx.tasty.{_, given _}

  val bTree = body.asTerm
  val under = bTree.underlyingArgument

  val res = '{Box(${under.asInstanceOf[Term].asExpr})}
  res
}

class Box(inner: => Any)
