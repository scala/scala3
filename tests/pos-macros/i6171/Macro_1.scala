import scala.quoted._

object scalatest {

  inline def assert(x: => Any): Unit = ${ assertImpl('x) }

  def assertImpl(x: Expr[Any])(using Quotes) : Expr[Unit] = {
    import qctx.reflect._
    Term.of(x).underlyingArgument
    '{ () }
  }
}
