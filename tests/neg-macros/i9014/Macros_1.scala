import scala.quoted._
trait Bar
inline given Bar = ${ impl }
def impl(using Quotes): Expr[Bar] = report.throwError("Failed to expand!")
