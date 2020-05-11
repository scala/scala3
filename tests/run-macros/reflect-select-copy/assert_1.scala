import scala.quoted._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(using s: Scope)(cond: s.Expr[Boolean], clue: s.Expr[Any]): s.Expr[Unit] = {
    import s.tasty._

    cond.underlyingArgument match {
      case Apply(select @ Select(lhs, op), rhs :: Nil) =>
        val cond = Apply(Select.copy(select)(lhs, ">"), rhs :: Nil).seal.cast[Boolean]
        '{ scala.Predef.assert($cond) }
      case _ =>
        '{ scala.Predef.assert($cond) }
    }
  }
}