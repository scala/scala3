import scala.quoted._
trait Bar
transparent inline given Bar = ${ impl }
def impl(using Quotes): Expr[Bar] = quotes.reflect.report.errorAndAbort("Failed to expand!")
