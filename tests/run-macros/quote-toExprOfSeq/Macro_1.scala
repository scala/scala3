import scala.quoted._
import delegate scala.quoted._

inline def seq = ${fooImpl}

def fooImpl given (qctx: QuoteContext) = {
  List('{1}, '{2}, '{3}).toExprOfSeq
}
