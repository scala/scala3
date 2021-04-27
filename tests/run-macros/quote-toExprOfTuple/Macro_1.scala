import scala.quoted.*

object Macro {
  inline def t2[T0, T1](t0: T0, t1: T1): (T0, T1) = ${ impl2('{t0}, '{t1}) }

  def impl2[T0: Type, T1: Type](t0: Expr[T0], t1: Expr[T1])(using Quotes) : Expr[(T0, T1)] = {
    import quotes.reflect.*
    import util.*

    val seq = List(t0, t1)
    val res = Expr.ofTupleFromSeq(seq)
    res.asExprOf[(T0, T1)]
  }
}
