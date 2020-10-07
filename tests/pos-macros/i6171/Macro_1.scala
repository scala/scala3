import scala.quoted._

object scalatest {

  inline def assert(x: => Any): Unit = ${ assertImpl('x) }

  def assertImpl(x: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    x.unseal.underlyingArgument
    '{ () }
  }
}
