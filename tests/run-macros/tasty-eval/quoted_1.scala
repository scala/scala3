import scala.quoted._
import scala.quoted.autolift._

object Macros {

  implicit inline def foo(i: Int): String =
    ${ impl('i) }

  def impl(i: Expr[Int]) given QuoteContext: Expr[String] = {
    value(i).toString
  }

  inline implicit def value[X](e: Expr[X])(implicit qctx: QuoteContext, ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(e: Expr[X]) given QuoteContext: Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(e: Expr[Int]) given (qctx: QuoteContext): Option[Int] = {
      import qctx.tasty._

      e.unseal.tpe match {
        case Type.SymRef(IsValDefSymbol(sym), pre) =>
          sym.tree.tpt.tpe match {
            case Type.ConstantType(Constant(i: Int)) => Some(i)
            case _ => None
          }
        case Type.ConstantType(Constant(i: Int)) => Some(i)
        case _ => None
      }
    }
  }
}
