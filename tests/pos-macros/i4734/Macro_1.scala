import scala.annotation.tailrec
import scala.quoted._

object Macros {
  inline def unrolledForeach(f: Int => Int): Int =
   ${unrolledForeachImpl('f)}

  def unrolledForeachImpl(f: Expr[Int => Int]): Staged[Int] = '{
    val size: Int = 5
    ($f)(3)
  }
}
