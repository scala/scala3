import scala.quoted.*
trait Bar
inline given Bar = ${ impl }
def impl(using Quotes): Expr[Bar] = quotes.reflect.report.throwError("Failed to expand!")
