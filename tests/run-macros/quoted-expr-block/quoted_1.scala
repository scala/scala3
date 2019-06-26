import scala.quoted._

inline def replicate(inline times: Int, code: => Any) = ${replicateImpl(times, 'code)}

private def replicateImpl(times: Int, code: Expr[Any]) given tasty.Reflection = {
  @annotation.tailrec def loop(n: Int, accum: List[Expr[Any]]): List[Expr[Any]] =
    if (n > 0) loop(n - 1, code :: accum) else accum
  Expr.block(loop(times, Nil), '{})

}
