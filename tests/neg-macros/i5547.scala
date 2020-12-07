import scala.quoted._

object scalatest {
  inline def assert2(condition: => Boolean): Unit =
    ${ assertImpl('condition, Value("")) } // error

  def assertImpl(condition: Expr[Boolean], clue: Expr[Any])(using Quotes): Expr[Unit] =
    '{}
}
