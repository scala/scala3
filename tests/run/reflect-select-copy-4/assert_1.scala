import scala.quoted._
import scala.tasty._

object scalatest {

  inline def assert(condition: => Boolean): Unit = ~assertImpl('(condition), '(""))

  def assertImpl(cond: Expr[Boolean], clue: Expr[Any])(implicit refl: Reflection): Expr[Unit] = {
    import refl._

    def unsafeLet(expr: Term)(body: Term.Ident => Term): Term = { // as proposed in #5567
      // fundamentally dangerous, do not do this at home
      val s = expr.seal
      ('{
        val x: ~s.tpe = ~s.expr
        ~body(('(x)).unseal.asInstanceOf[Term.Ident]).seal.asExprOf[Any]
      }).unseal
    }

    cond.unseal.underlyingArgument match {
      case Term.Apply(Term.IsSelect(sel @ Term.Select(lhs, op)), rhs :: Nil) =>
        val cond = unsafeLet(lhs) { lhs2 =>
          unsafeLet(rhs) { rhs2 =>
            Term.Apply(Term.Select.copy(sel)(lhs2, op), rhs2 :: Nil)
          }
        }.seal.asExprOf[Boolean]
        '{ scala.Predef.assert(~cond) }
      case _ =>
        '{ scala.Predef.assert(~cond) }
    }
  }

}