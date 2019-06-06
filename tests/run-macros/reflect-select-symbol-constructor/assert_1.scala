import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ${ assertImpl('condition, '{""}) }

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._
    import util._

    def isImplicitMethodType(tp: Type): Boolean =
      Type.IsMethodType.unapply(tp).flatMap(tp => if tp.isImplicit then Some(true) else None).nonEmpty

    cond.unseal.underlyingArgument match {
      case t @ Apply(sel @ Select(lhs, op), rhs :: Nil) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Apply(Select(left, sel.symbol), right :: Nil)
            let(app) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.unseal
            }
          }
        }.seal.cast[Unit]
      case Apply(f @ Apply(sel @ Select(Apply(qual, lhs :: Nil), op), rhs :: Nil), implicits)
      if isImplicitMethodType(f.tpe) =>
        let(lhs) { left =>
          let(rhs) { right =>
            val app = Apply(Select(Apply(qual, left :: Nil), sel.symbol), right :: Nil)
            let(Apply(app, implicits)) { result =>
              val l = left.seal
              val r = right.seal
              val b = result.seal.cast[Boolean]
              val code = '{ scala.Predef.assert($b) }
              code.unseal
            }
          }
        }.seal.cast[Unit]
    }
  }

}
