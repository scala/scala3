import scala.quoted._

object scalatest {
  inline def assert1(condition: => Boolean): Unit =
   ${assertImpl('condition, '{""})}

  inline def assert2(condition: => Boolean): Unit =
    ${ assertImpl('condition, '{""}) }

  def assertImpl(using Scope)(condition: scope.Expr[Boolean], clue: scope.Expr[Any]): scope.Expr[Unit] =
    '{}
}
