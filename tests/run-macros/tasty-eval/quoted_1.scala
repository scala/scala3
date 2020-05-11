import scala.quoted._

object Macros {

  implicit inline def foo(i: Int): String =
    ${ impl('i) }

  def impl(using s: Scope)(i: s.Expr[Int]): s.Expr[String] = {
    Expr(value(i).toString)
  }

  inline implicit def value[X](using s: Scope)(e: s.Expr[X])(implicit ev: Valuable[X]): Option[X] = ev.value(e)

  trait Valuable[X] {
    def value(using s: Scope)(e: s.Expr[X]): Option[X]
  }

  implicit def intIsEvalable: Valuable[Int] = new Valuable[Int] {
    override def value(using s: Scope)(e: s.Expr[Int]) : Option[Int] = {
      import s.tasty._

      e.tpe match {
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
