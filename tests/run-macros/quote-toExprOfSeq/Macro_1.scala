import scala.quoted._

inline def seq = ${fooImpl}

def fooImpl(using qctx: QuoteContext) = {
  Expr.ofSeq(List('{1}, '{2}, '{3}))
}
