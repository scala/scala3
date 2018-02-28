import scala.quoted._

object Test {
//  def compile(env: Expr[Int]): Expr[Int] = '(3)


  def main(args: Array[String]): Unit = {
    '{ (x: Int) => ~('(x)) }
  }
}
