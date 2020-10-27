import scala.quoted._
trait Bar
inline given Bar = ${ impl }
def impl(using qctx: QuoteContext): Expr[Bar] = report.throwError("Failed to expand!")
