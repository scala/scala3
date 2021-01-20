import scala.quoted._

object Succ with

  inline def unapply(n: Int): Option[Int] = ${ impl('n) }

  private def impl(n: Expr[Int])(using Quotes): Expr[Option[Int]] =
    '{ if $n == 0 then None else Some($n - 1)}
