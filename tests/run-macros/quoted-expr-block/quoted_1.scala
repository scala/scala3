import scala.quoted._

inline def replicate(inline times: Int, code: => Any) = ${replicateImpl('times, 'code)}

private def replicateImpl(times: Expr[Int], code: Expr[Any]) with QuoteContext = {
  @annotation.tailrec def loop(n: Int, accum: List[Expr[Any]]): List[Expr[Any]] =
    if (n > 0) loop(n - 1, code :: accum) else accum
  Expr.block(loop(times.value, Nil), '{})
}
