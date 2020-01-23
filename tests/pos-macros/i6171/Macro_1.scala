import scala.quoted._

object scalatest {

  inline def assert(x: => Any): Unit = ${ assertImpl('x) }

  def assertImpl(x: Expr[Any]) with (qctx: QuoteContext) : Expr[Unit] = {
    import qctx.tasty.{_, given _}
    x.unseal.underlyingArgument
    '{ () }
  }
}
