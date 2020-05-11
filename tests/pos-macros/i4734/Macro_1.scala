import scala.annotation.tailrec
import scala.quoted._

object Macros {
  inline def unrolledForeach(f: Int => Int): Int =
    ${unrolledForeachImpl('f)}

  def unrolledForeachImpl(using s: Scope)(f: s.Expr[Int => Int]): s.Expr[Int] = '{
    val size: Int = 5
    ($f)(3)
  }
}
