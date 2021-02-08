
import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit =
    val s1: Quotes ?=> Expr[Int] = {
      given Compiler = Compiler.make(getClass.getClassLoader)
      run[Quotes ?=> Expr[Int]] { stage1('{2}) }
    }
    {
      given Compiler = Compiler.make(getClass.getClassLoader)
      println(run(s1))
    }
  def stage1(x: Expr[Int])(using Quotes): Expr[Quotes ?=> Expr[Int]] =
    val code = '{ (q1: Quotes) ?=>
      val x1 = $x
      '{ 1 + ${Expr(x1)} }
    }
    println("stage1 code: " + code.show)
    code

}
