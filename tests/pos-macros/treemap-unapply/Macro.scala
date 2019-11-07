import scala.quoted.{ given, _ }

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(x: Expr[Unit])(given ctx: QuoteContext): Expr[Unit] =
  import ctx.tasty.{ given, _ }
  val tr: Term = x.unseal
  object m extends TreeMap
  m.transformTerm(tr).seal.cast[Unit]
