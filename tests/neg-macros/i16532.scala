import scala.quoted.*

def power0Impl(a: Expr[Int], b: Expr[Int])(using Quotes): Expr[Int] =
  inline def recurseII(a:Int, n:Int): Int = ???

  '{
    val x2 = recurseII($a, $b) // error
    x2
   }
