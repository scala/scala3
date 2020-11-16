import scala.quoted._

object Macros {

  implicit inline def foo(i: Int): String =
    ${ impl('i) }

  def impl(i: Expr[Int]) (using QuoteContext): Expr[String] = {
    Expr(value(i).toString)
  }

  inline implicit def value[X](e: Expr[X])(implicit qctx: QuoteContext, ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(e: Expr[X]) (using QuoteContext): Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(e: Expr[Int])(using qctx: QuoteContext) : Option[Int] = {
      import qctx.reflect._

      Term.of(e).tpe match {
        case pre: TermRef if pre.termSymbol.isValDef =>
          pre.termSymbol.tree match
            case t: ValDef =>
              t.tpt.tpe match {
                case ConstantType(Constant.Int(i)) => Some(i)
                case _ => None
              }
        case ConstantType(Constant.Int(i)) => Some(i)
        case _ => None
      }
    }
  }
}
