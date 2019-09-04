import scala.quoted._

object Macro {

  trait AddInt[A <: Int, B <: Int] { type Out <: Int }

  inline def ff[A <: Int, B <: Int]() <: AddInt[A, B] = ${ impl('[A], '[B]) }

  def impl[A <: Int : Type, B <: Int : Type](a: Type[A], b: Type[B]) given (qctx: QuoteContext): Expr[AddInt[A, B]] = {
    import qctx.tasty._

    val Type.ConstantType(Constant(v1: Int)) = a.unseal.tpe
    val Type.ConstantType(Constant(v2: Int)) = b.unseal.tpe

    val t = Literal(Constant((v1 + v2): Int)).tpe.seal

    '{ null: AddInt[$a, $b] { type Out = $t } }
  }
}
