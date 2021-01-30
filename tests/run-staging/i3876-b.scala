import scala.quoted.*
import scala.quoted.staging.*
object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)

    def x(using Quotes): Expr[Int] = '{3}

    def f2(using Quotes): Expr[Int => Int] = '{
      def f(x: Int): Int = x + x
      f
    }

    def expr(using Quotes) = '{$f2($x)}
    println(run(Expr.betaReduce(expr)))
    println(withQuotes(Expr.betaReduce(expr).show)) // TODO improve printer
  }
}
