import scala.quoted.*

object scalatest {
  inline def assert1(condition: => Boolean): Unit =
    ${assertImpl('condition, '{""})}

  def assertImpl(condition: Expr[Boolean], clue: Expr[Any])(using Quotes): Expr[Unit] =
    '{}
}
