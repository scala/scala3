import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    import quoted.Toolbox.Default._

    cond.unseal.underlyingArgument match {
      case Term.Apply(sel @ Term.Select(lhs, op), rhs :: Nil) =>
        val Term.IsSelect(select) = sel
        val cond = Term.Apply(Term.Select.copy(select)(lhs, ">"), rhs :: Nil).seal[Boolean]
        '{ scala.Predef.assert($cond) }
      case _ =>
        '{ scala.Predef.assert($cond) }
    }
  }
}