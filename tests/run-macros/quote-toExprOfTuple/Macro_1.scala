import scala.quoted._

object Macro {
  inline def t2[T0, T1](t0: T0, t1: T1): (T0, T1) = ${ impl2('{t0}, '{t1}) }

  def impl2[T0: Type, T1: Type](t0: Expr[T0], t1: Expr[T1]) given (qctx: QuoteContext): Expr[(T0, T1)] = {
    import qctx.tasty._
    import util._

    val seq = List(t0, t1)
    val res = seq.toExprOfTuple
    res.cast[(T0, T1)]
  }
}
