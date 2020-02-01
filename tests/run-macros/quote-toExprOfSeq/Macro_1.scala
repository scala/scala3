import scala.quoted._
import scala.quoted.{given _}

inline def seq = ${fooImpl}

def fooImpl(using qctx: QuoteContext) = {
  Expr.ofSeq(List('{1}, '{2}, '{3}))
}
