import scala.quoted._

inline def mcr(body: => Any): Unit = ${mcrImpl('body)}

def mcrImpl[T](body: Expr[Any])(using ctx: QuoteContext) : Expr[Any] = {
  import ctx.tasty.{_, given}

  val bTree = body.unseal
  val under = bTree.underlyingArgument

  val res = '{Box(${under.asInstanceOf[Term].seal})}
  res
}

class Box(inner: => Any)
