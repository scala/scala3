import scala.quoted._

object Test {
  def foo1(n: List[Int]): Expr[List[Int]] = '(n) // error
  def foo2(n: Test.type): Expr[Test.type] = '(n) // error
  def foo3(n: Int): Expr[Int => Expr[Int]] = '(m => '(n + m)) // error
}
