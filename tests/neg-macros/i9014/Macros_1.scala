import scala.quoted._
trait Bar
inline given as Bar = ${ impl }
def impl(using s: Scope): s.Expr[Bar] = report.throwError("Failed to expand!")
