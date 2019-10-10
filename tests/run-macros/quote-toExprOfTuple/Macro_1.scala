import scala.quoted._

object Macro {
  inline def t2[T0, T1](t0: T0, t1: T1): (T0, T1) = ${ impl2('{t0}, '{t1}) }

  def impl2[T0: TypeTag, T1: TypeTag](t0: Expr[T0], t1: Expr[T1])(given qctx: QuoteContext): Expr[(T0, T1)] = {
    import qctx.tasty.{_, given}
    import util._

    val seq = List(t0, t1)
    val res = Expr.ofTuple(seq)
    res.cast[(T0, T1)]
  }
}
