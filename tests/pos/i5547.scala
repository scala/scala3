import scala.quoted._

object scalatest {
  inline def assert1(condition: => Boolean): Unit =
   ${assertImpl('condition, '{""})}

  inline def assert2(condition: => Boolean): Unit =
    ${ assertImpl('condition, "".toExpr) }

  def assertImpl(condition: Expr[Boolean], clue: Expr[Any]) given QuoteContext: Expr[Unit] =
    '{}
}
