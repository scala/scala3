import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    def x(using s: Scope): s.Expr[Int] = '{3}

    def f(using s: Scope): s.Expr[Int => Int] = '{ (x: Int) => x + x }

    println(run(Expr.betaReduce('{$f($x)})))
    println(usingNewScope(Expr.betaReduce('{$f($x)}).show))
  }
}
