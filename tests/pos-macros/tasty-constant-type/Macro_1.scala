import scala.quoted._

object Macro {

  trait AddInt[A <: Int, B <: Int] { type Out <: Int }

  transparent inline def ff[A <: Int, B <: Int](): AddInt[A, B] = ${ impl('[A], '[B]) }

  def impl[A <: Int : Type, B <: Int : Type](a: Type[A], b: Type[B])(using qctx: QuoteContext) : Expr[AddInt[A, B]] = {
    import qctx.reflect._

    val ConstantType(Constant.Int(v1)) = a.unseal.tpe
    val ConstantType(Constant.Int(v2)) = b.unseal.tpe

    Literal(Constant.Int(v1 + v2)).tpe.seal match
      case '[$t] => '{ null: AddInt[$a, $b] { type Out = $t } }
  }
}
