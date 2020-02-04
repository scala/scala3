import scala.quoted.{ given _, _ }

inline def mcr(x: => Unit): Unit = ${mcrImpl('x)}
def mcrImpl(x: Expr[Unit])(using qctx: QuoteContext) : Expr[Unit] =
  import qctx.tasty.{ given _, _ }
  val tr: Term = x.unseal
  object m extends scala.tasty.reflect.TreeMap {
    val reflect: qctx.tasty.type = qctx.tasty
  }
  m.transformTerm(tr).seal.cast[Unit]
