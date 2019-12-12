import scala.quoted._

object Macro {

  inline def openTest(x: => Any): Any = ${ Macro.impl('x) }

  def impl(x: Expr[Any])(given QuoteContext): Expr[Any] = {
    x match {
      case '{ (x: Int) => ($body: Int => Int)(x) } => Expr.open(body) { (body, close) => close(body)(Expr(2)) }
      case '{ (x1: Int, x2: Int) => ($body: (Int, Int) => Int)(x1, x2) } => Expr.open(body) { (body, close) => close(body)(Expr(2), Expr(3)) }
      case '{ (x1: Int, x2: Int, x3: Int) => ($body: (Int, Int, Int) => Int)(x1, x2, x3) } => Expr.open(body) { (body, close) => close(body)(Expr(2), Expr(3), Expr(4)) }
    }
  }

}
