import scala.quoted._

object Macro {

  trait AddInt[A <: Int, B <: Int] { type Out <: Int }

  inline def ff[A <: Int, B <: Int]() <: AddInt[A, B] = ${ impl('[A], '[B]) }

  def impl[A <: Int : Type, B <: Int : Type](a: Type[A], b: Type[B])(implicit r: tasty.Reflection): Expr[AddInt[A, B]] = {
    import r._

    val Type.ConstantType(Constant.Int(v1)) = a.unseal.tpe
    val Type.ConstantType(Constant.Int(v2)) = b.unseal.tpe

    val t = Literal(Constant.Int(v1 + v2)).tpe.seal

    '{ null: AddInt[$a, $b] { type Out = $t } }
  }
}
