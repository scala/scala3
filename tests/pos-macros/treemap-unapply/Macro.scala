import scala.quoted.{ given _, _ }

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(x: Expr[Unit]) with (ctx: QuoteContext) : Expr[Unit] =
  import ctx.tasty.{ given _, _ }
  val tr: Term = x.unseal
  object m extends TreeMap
  m.transformTerm(tr).seal.cast[Unit]
