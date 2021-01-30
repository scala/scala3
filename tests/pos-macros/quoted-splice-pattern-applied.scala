import scala.quoted.*

def f(x: Expr[Int])(using Quotes) = x match {
  case '{ ${f}($a: Int): Int } =>
    val f1: Expr[Int => Int] = f
    val a1: Expr[Int] = a
  case '{ def a: Int = ${f}($b: Int); () } =>
    val f1: Expr[Int => Int] = f
    val b1: Expr[Int] = b
  case '{ val a: Int = 3; ${f}(a): Int } =>
    val f1: Expr[Int => Int] = f
}
