import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ~assertImpl('(condition), '(""))

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    import util._
    import quoted.Toolbox.Default._

    def isImplicitMethodType(tp: Type): Boolean =
      Type.IsMethodType.unapply(tp).flatMap(tp => if tp.isImplicit then Some(true) else None).nonEmpty

    cond.unseal.underlyingArgument match {
      case Term.Apply(sel @ Term.Select(lhs, op), rhs :: Nil) =>
        let(lhs)(definitions.UnitType) { left =>
          let(rhs)(definitions.UnitType) { right =>
            let(Term.Apply(Term.Select.copy(sel)(left, op), right :: Nil))(definitions.UnitType) { result =>
              val l = left.seal[Any]
              val r = right.seal[Any]
              val b = result.seal[Boolean]
              val code = '{ scala.Predef.assert(~b) }
              code.unseal
            }
          }
        }.seal[Unit]
      case Term.Apply(f @ Term.Apply(Term.IsSelect(sel @ Term.Select(Term.Apply(qual, lhs :: Nil), op)), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        let(lhs)(definitions.UnitType) { left =>
          let(rhs)(definitions.UnitType) { right =>
            let(Term.Apply(Term.Apply(Term.Select.copy(sel)(Term.Apply(qual, left :: Nil), op), right :: Nil), implicits))(definitions.UnitType) { result =>
              val l = left.seal[Any]
              val r = right.seal[Any]
              val b = result.seal[Boolean]
              val code = '{ scala.Predef.assert(~b) }
              code.unseal
            }
          }
        }.seal[Unit]
    }
  }

}
