import scala.quoted._

object lib {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using qctx: QuoteContext) : Expr[Unit] = {
    import qctx.reflect._
    import util._

    Term.of(cond).underlyingArgument match {
      case t @ Apply(Select(lhs, op), Lambda(param :: Nil, Apply(Select(a, "=="), b :: Nil)) :: Nil)
      if a.symbol == param.symbol || b.symbol == param.symbol =>
        '{ scala.Predef.assert($cond) }
    }
  }
}
