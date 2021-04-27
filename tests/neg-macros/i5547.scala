import scala.quoted.*

object scalatest {
  inline def assert2(condition: => Boolean): Unit =
    ${ assertImpl('condition, Expr("")) } // error

  def assertImpl(condition: Expr[Boolean], clue: Expr[Any])(using Quotes): Expr[Unit] =
    '{}
}
