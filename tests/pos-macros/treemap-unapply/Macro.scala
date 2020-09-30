import scala.quoted._

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(x: Expr[Unit])(using ctx: QuoteContext) : Expr[Unit] =
  import ctx.tasty._
  val tr: Term = x.unseal
  object m extends TreeMap
  m.transformTerm(tr).seal.cast[Unit]
