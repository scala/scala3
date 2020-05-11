import scala.quoted._

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(using s: Scope)(x: s.Expr[Unit]): s.Expr[Unit] =
  import s.tasty._
  val tr: Term = x
  object m extends TreeMap
  given s.Type[Unit] = '[Unit] // FIXME remove
  m.transformTerm(tr).seal.cast[Unit]
