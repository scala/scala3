import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ~assertImpl('(condition), '(""))

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._

    cond match {
      case BinaryApplication(lhs, rhs, f) =>
        '{
          val left: ~lhs.tpe = ~lhs.expr
          val right: ~rhs.tpe = ~rhs.expr
          scala.Predef.assert(~f('(left), '(right)))
        }
      case _ =>
        '{ scala.Predef.assert(~cond) }
    }
  }

  object BinaryApplication {
    // TODO add dependent types to function parameters (not currently possible with this API)
    def unapply[T: Type](arg: Expr[T])(implicit refl: Reflection): Option[(Sealed, Sealed, (Expr[_], Expr[_]) => Expr[T])] = {
      import refl._
      arg.unseal.underlyingArgument match {
        case Term.Apply(Term.IsSelect(sel @ Term.Select(lhs, op)), rhs :: Nil) =>
          val left = lhs.seal
          val right = rhs.seal
          val application = (lhs: Expr[_], rhs: Expr[_]) => Term.Apply(Term.Select.copy(sel)(lhs.unseal, op), rhs.unseal :: Nil).seal.asExprOf[T]
          Some((left, right, application))
        case _ =>
          None
      }
    }

  }
}