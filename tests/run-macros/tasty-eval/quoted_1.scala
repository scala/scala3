import scala.quoted._
import scala.quoted.autolift.{given _}

object Macros {

  implicit inline def foo(i: Int): String =
    ${ impl('i) }

  def impl(i: Expr[Int]) with QuoteContext : Expr[String] = {
    value(i).toString
  }

  inline implicit def value[X](e: Expr[X])(implicit qctx: QuoteContext, ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(e: Expr[X]) with QuoteContext : Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(e: Expr[Int]) with (qctx: QuoteContext) : Option[Int] = {
      import qctx.tasty.{_, given _}

      e.unseal.tpe match {
        case pre: TermRef if pre.termSymbol.isValDef =>
          pre.termSymbol.tree match
            case t: ValDef =>
              t.tpt.tpe match {
                case ConstantType(Constant(i: Int)) => Some(i)
                case _ => None
              }
        case ConstantType(Constant(i: Int)) => Some(i)
        case _ => None
      }
    }
  }
}
