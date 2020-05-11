import scala.quoted._

inline def replicate(inline times: Int, code: => Any) = ${replicateImpl('times, 'code)}

private def replicateImpl(using s: Scope)(times: s.Expr[Int], code: s.Expr[Any]) = {
  @annotation.tailrec def loop(n: Int, accum: List[s.Expr[Any]]): List[s.Expr[Any]] =
    if (n > 0) loop(n - 1, code :: accum) else accum
  Expr.block(loop(times.unliftOrError, Nil), '{})
}
