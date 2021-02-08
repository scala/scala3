import scala.quoted.*

object Macros {

  implicit inline def foo(i: Int): String =
    ${ impl('i) }

  def impl(i: Expr[Int]) (using Quotes): Expr[String] = {
    Expr(value(i).toString)
  }

  inline implicit def value[X](e: Expr[X])(implicit qctx: Quotes, ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(e: Expr[X]) (using Quotes): Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(e: Expr[Int])(using Quotes) : Option[Int] = {
      import quotes.reflect.*

      e.asTerm.tpe match {
        case pre: TermRef if pre.termSymbol.isValDef =>
          pre.termSymbol.tree match
            case t: ValDef =>
              t.tpt.tpe match {
                case ConstantType(IntConstant(i)) => Some(i)
                case _ => None
              }
        case ConstantType(IntConstant(i)) => Some(i)
        case _ => None
      }
    }
  }
}
