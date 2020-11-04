import scala.quoted._

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(x: Expr[Unit])(using QuoteContext) : Expr[Unit] =
  import qctx.reflect._
  val tr: Term = x.unseal
  object m extends TreeMap
  m.transformTerm(tr).seal.cast[Unit]
