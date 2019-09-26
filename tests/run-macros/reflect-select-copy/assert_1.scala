import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(given qctx: QuoteContext): Expr[Unit] = {
    import qctx.tasty.{_, given}

    cond.unseal.underlyingArgument match {
      case Apply(select @ Select(lhs, op), rhs :: Nil) =>
        val cond = Apply(Select.copy(select)(lhs, ">"), rhs :: Nil).seal.cast[Boolean]
        '{ scala.Predef.assert($cond) }
      case _ =>
        '{ scala.Predef.assert($cond) }
    }
  }
}