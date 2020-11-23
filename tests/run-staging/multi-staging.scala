
import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit =
    val s1: Quotes ?=> Expr[Int] = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run[Quotes ?=> Expr[Int]] { stage1('{2}) }
    }
    {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      println(run(s1))
    }
  def stage1(x: Expr[Int])(using Quotes): Expr[Quotes ?=> Expr[Int]] =
    val code = '{ (using q1: Quotes) =>
      val x1 = $x
      '{ 1 + ${Expr(x1)} }
    }
    println("stage1 code: " + code.show)
    code

}
