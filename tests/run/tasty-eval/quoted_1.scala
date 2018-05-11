import scala.quoted._

import scala.tasty.Universe
import scala.tasty.util.{TastyPrinter, TreeTraverser}

object Macros {

  implicit inline def foo(i: Int): String =
    ~impl('(i))(Universe.compilationUniverse) // FIXME infer Universe.compilationUniverse within top level ~

  def impl(i: Expr[Int])(implicit u: Universe): Expr[String] = {
    value(i).toString.toExpr
  }

  inline implicit def value[X](e: Expr[X])(implicit u: Universe, ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(e: Expr[X])(implicit u: Universe): Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(e: Expr[Int])(implicit u: Universe): Option[Int] = {
      import u._
      import u.tasty._
      e.toTasty.tpe match {
        case SymRef(ValDef(_, tpt, _), pre) =>
          tpt.tpe match {
            case ConstantType(Constant.Int(i)) => Some(i)
            case _ => None
          }
        case ConstantType(Constant.Int(i)) => Some(i)
        case _ => None
      }
    }
  }
}
