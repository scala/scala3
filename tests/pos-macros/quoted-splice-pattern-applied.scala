import scala.quoted._

def f(using s: Scope)(x: s.Expr[Int]) = x match {
  case '{ ${f}($a: Int): Int } =>
    val f1: s.Expr[Int => Int] = f
    val a1: s.Expr[Int] = a
  case '{ def a: Int = ${f}($b: Int); () } =>
    val f1: s.Expr[Int => Int] = f
    val b1: s.Expr[Int] = b
  case '{ val a: Int = 3; ${f}(a): Int } =>
    val f1: s.Expr[Int => Int] = f
}
