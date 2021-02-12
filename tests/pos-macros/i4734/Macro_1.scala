import scala.annotation.tailrec
import scala.quoted.*

object Macros {
  inline def unrolledForeach(f: Int => Int): Int =
    ${unrolledForeachImpl('f)}

  def unrolledForeachImpl(f: Expr[Int => Int])(using Quotes): Expr[Int] = '{
    val size: Int = 5
    ($f)(3)
  }
}
