import scala.quoted._

object Macro {

  trait AddInt[A <: Int, B <: Int] { type Out <: Int }

  transparent inline def ff[A <: Int, B <: Int](): AddInt[A, B] = ${ impl(Type[A], Type[B]) }

  def impl[A <: Int : Type, B <: Int : Type](a: Type[A], b: Type[B])(using qctx: QuoteContext) : Expr[AddInt[A, B]] = {
    import qctx.tasty._

    val ConstantType(Constant(v1: Int)) = a.unseal.tpe
    val ConstantType(Constant(v2: Int)) = b.unseal.tpe

    Literal(Constant((v1 + v2): Int)).tpe.seal match
      case '[$t] => '{ null: AddInt[a.T, b.T] { type Out = t.T } }
  }
}
