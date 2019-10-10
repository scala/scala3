import scala.quoted._
import scala.quoted.autolift.given

object Macros {

  implicit inline def foo(i: Int): String =
    ${ impl('i) }

  def impl(i: Expr[Int])(given QuoteContext): Expr[String] = {
    value(i).toString
  }

  inline implicit def value[X](e: Expr[X])(implicit qctx: QuoteContext, ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(e: Expr[X])(given QuoteContext): Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(e: Expr[Int])(given qctx: QuoteContext): Option[Int] = {
      import qctx.tasty.{_, given}

      e.unseal.tpe match {
        case Type.IsTermRef(pre) if pre.termSymbol.isValDef =>
          val IsValDef(t) = pre.termSymbol.tree
          t.tpt.tpe match {
            case Type.ConstantType(Constant(i: Int)) => Some(i)
            case _ => None
          }
        case Type.ConstantType(Constant(i: Int)) => Some(i)
        case _ => None
      }
    }
  }
}
