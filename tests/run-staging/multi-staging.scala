
import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit =
    val s1: (s: Scope) ?=> s.Expr[Int] = {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      run[(s: Scope) ?=> s.Expr[Int]] { stage1('{2}) }
    }
    {
      given Toolbox = Toolbox.make(getClass.getClassLoader)
      println(run(s1))
    }
  def stage1(using s1: Scope)(x: s1.Expr[Int]): s1.Expr[(s2: Scope) ?=> s2.Expr[Int]] =
    val code = '{ (using s2: Scope) =>
      val x1 = $x
      '{ 1 + ${Expr(x1)} }
    }
    println("stage1 code: " + code.show)
    code

}
