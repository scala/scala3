import scala.quoted.*
import scala.quoted.staging.*
object Test {
  def main(args: Array[String]): Unit = {
    implicit def toolbox: scala.quoted.staging.Compiler = scala.quoted.staging.Compiler.make(getClass.getClassLoader)

    def x(using Quotes): Expr[Int] = '{3}

    def f3(using Quotes): Expr[Int => Int] = '{
      val f: (x: Int) => Int = x => x + x
      f
    }
    def expr(using Quotes) = '{$f3($x)}
    println(run(Expr.betaReduce(expr)))
    println(withQuotes(Expr.betaReduce(expr).show)) // TODO improve printer
  }
}
