import scala.quoted._

object Macro {

  trait AddInt[A <: Int, B <: Int] { type Out <: Int }

  transparent inline def ff[A <: Int, B <: Int](): AddInt[A, B] = ${ impl('[A], '[B]) }

  def impl[A <: Int : Staged, B <: Int : Staged](a: Staged[A], b: Staged[B])(using qctx: QuoteContext) : Expr[AddInt[A, B]] = {
    import qctx.tasty._

    val ConstantType(Constant(v1: Int)) = a.unseal.tpe
    val ConstantType(Constant(v2: Int)) = b.unseal.tpe

    Literal(Constant((v1 + v2): Int)).tpe.seal match
      case '[$t] => '{ null: AddInt[$a, $b] { type Out = $t } }
  }
}
