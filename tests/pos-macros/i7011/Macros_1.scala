import scala.quoted._, scala.quoted.matching._
import scala.quoted.given

inline def mcr(body: => Any): Unit = ${mcrImpl('body)}

def mcrImpl[T](body: Expr[Any])(given ctx: QuoteContext): Expr[Any] = {
  import ctx.tasty._

  val bTree = body.unseal
  val under = bTree.underlyingArgument

  val res = '{Box(${under.asInstanceOf[Term].seal})}
  res
}

class Box(inner: => Any)
