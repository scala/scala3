import scala.quoted._

object Succ:

  inline def unapply(n: Int): Option[Int] = ${ impl('n) }

  private def impl(using s: Scope)(n: s.Expr[Int]): s.Expr[Option[Int]] =
    '{ if $n == 0 then None else Some($n - 1)}
