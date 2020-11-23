import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)

    def x(using Quotes): Expr[Int] = '{3}

    def f(using Quotes): Expr[Int => Int] = '{ (x: Int) => x + x }

    println(run(Expr.betaReduce('{$f($x)})))
    println(withQuotes(Expr.betaReduce('{$f($x)}).show))
  }
}
