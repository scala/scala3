import scala.quoted._

object Macro {

  trait AddInt[A <: Int, B <: Int] { type Out <: Int }

  transparent inline def ff[A <: Int, B <: Int](): AddInt[A, B] = ${ impl('[A], '[B]) }

  def impl[A <: Int, B <: Int](using s: Scope)(a: s.Type[A], b: s.Type[B])(using s.Type[A], s.Type[B]): s.Expr[AddInt[A, B]] = {
    import s.tasty._

    val ConstantType(Constant(v1: Int)) = a.tpe
    val ConstantType(Constant(v2: Int)) = b.tpe

    Literal(Constant((v1 + v2): Int)).tpe.seal.get match
      case '[$t] => '{ null: AddInt[$a, $b] { type Out = $t } }
  }
}
