import scala.quoted._
import scala.quoted.given

inline def seq = ${fooImpl}

def fooImpl(given qctx: QuoteContext) = {
  List('{1}, '{2}, '{3}).toExprOfSeq
}
