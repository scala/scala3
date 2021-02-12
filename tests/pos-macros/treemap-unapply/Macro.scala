import scala.quoted.*

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(x: Expr[Unit])(using Quotes) : Expr[Unit] =
  import quotes.reflect.*
  val tr: Term = x.asTerm
  object m extends TreeMap
  m.transformTerm(tr)(Symbol.spliceOwner).asExprOf[Unit]
