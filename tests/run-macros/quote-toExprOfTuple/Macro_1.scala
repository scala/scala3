import scala.quoted._

object Macro {
  inline def t2[T0, T1](t0: T0, t1: T1): (T0, T1) = ${ impl2('{t0}, '{t1}) }

  def impl2[T0, T1](using s: Scope)(t0: s.Expr[T0], t1: s.Expr[T1])(using s.Type[T0], s.Type[T1]): s.Expr[(T0, T1)] = {
    import s.tasty._
    import util._

    val seq = List[s.Expr[Any]](t0, t1)
    val res = Expr.ofTupleFromSeq(seq)
    res.cast[(T0, T1)]
  }
}
