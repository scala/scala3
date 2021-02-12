import scala.quoted.*

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(using Quotes) : Expr[Unit] = {
    import quotes.reflect.*

    cond.asTerm.underlyingArgument match {
      case app @ Apply(select @ Select(lhs, op), rhs :: Nil) =>
        val cond = Apply(Select.copy(select)(lhs, "exists"), rhs :: Nil).asExprOf[Boolean]
        '{ scala.Predef.assert($cond) }
      case _ =>
        '{ scala.Predef.assert($cond) }
    }
  }
}
