import scala.quoted.*

object Macros:
  def impl(using Quotes): Expr[Int] =
    val inner: Expr[Int] = '{ 1 }
    val pair: (Int, Expr[Int]) = mkPair(0, '{ $inner + 1 })
    pair._2

  inline def trigger: Int = ${ impl }
