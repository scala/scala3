import scala.quoted._
trait Bar
inline given as Bar = ${ impl }
def impl(using qctx: QuoteContext): Expr[Bar] = Reporting.throwError("Failed to expand!")
