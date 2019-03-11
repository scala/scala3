import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    import util._
    import quoted.Toolbox.Default._

    def isImplicitMethodType(tp: Type): Boolean =
      Type.IsMethodType.unapply(tp).flatMap(tp => if tp.isImplicit then Some(true) else None).nonEmpty

    cond.unseal.underlyingArgument match {
      case t @ Term.Apply(Term.Select(lhs, op), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Term.Select.overloaded(left, op, Nil, right :: Nil)
            let(app) { result =>
              val l = left.seal[Any]
              val r = right.seal[Any]
              val b = result.seal[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.unseal
            }
          }
        }.seal[Unit]
      case Term.Apply(f @ Term.Apply(Term.Select(Term.Apply(qual, lhs :: Nil), op), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Term.Select.overloaded(Term.Apply(qual, left :: Nil), op, Nil, right :: Nil)
            let(Term.Apply(app, implicits)) { result =>
              val l = left.seal[Any]
              val r = right.seal[Any]
              val b = result.seal[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.unseal
            }
          }
        }.seal[Unit]
    }
  }

}
