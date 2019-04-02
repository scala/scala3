import scala.quoted._
import scala.quoted.autolift._

import scala.tasty._

object Macros {

  implicit inline def foo(i: Int): String =
    ${ impl('i) }

  def impl(i: Expr[Int])(implicit reflect: Reflection): Expr[String] = {
    value(i).toString
  }

  inline implicit def value[X](e: Expr[X])(implicit reflect: Reflection, ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(e: Expr[X])(implicit reflect: Reflection): Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(e: Expr[Int])(implicit reflect: Reflection): Option[Int] = {
      import reflect._

      e.unseal.tpe match {
        case Type.SymRef(IsValDefSymbol(sym), pre) =>
          sym.tree.tpt.tpe match {
            case Type.ConstantType(Constant.Int(i)) => Some(i)
            case _ => None
          }
        case Type.ConstantType(Constant.Int(i)) => Some(i)
        case _ => None
      }
    }
  }
}
