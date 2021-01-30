import scala.quoted.*

object scalatest {

  inline def assert(x: => Any): Unit = ${ assertImpl('x) }

  def assertImpl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*
    x.asTerm.underlyingArgument
    '{ () }
  }
}
