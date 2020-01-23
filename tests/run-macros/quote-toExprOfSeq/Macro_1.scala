import scala.quoted._
import scala.quoted.{given _}

inline def seq = ${fooImpl}

def fooImpl with (qctx: QuoteContext) = {
  Expr.ofSeq(List('{1}, '{2}, '{3}))
}
