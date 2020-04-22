import scala.quoted._
import scala.quoted.unsafe._
object Macro {

  inline def openTest(inline x: Any): Any = ${ Macro.impl('x) }

  def impl(x: Expr[Any])(using QuoteContext): Expr[Any] = {
    x match {
      case '{ (x: Int) => $body(x): Int } => UnsafeExpr.open(body) { (body, close) => close(body)(Expr(2)) }
      case '{ (x1: Int, x2: Int) => $body(x1, x2): Int } => UnsafeExpr.open(body) { (body, close) => close(body)(Expr(2), Expr(3)) }
      case '{ (x1: Int, x2: Int, x3: Int) => $body(x1, x2, x3): Int } => UnsafeExpr.open(body) { (body, close) => close(body)(Expr(2), Expr(3), Expr(4)) }
    }
  }

}
