import scala.quoted._

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(x: Expr[Unit])(using ctx: QuoteContext) : Expr[Unit] =
  import ctx.tasty.{ given _, _ }
  val tr: Term = x.asTerm
  object m extends TreeMap
  m.transformTerm(tr).asExprOf[Unit]
