import scala.quoted.*
import scala.quoted.staging.*
object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)

    def x(using Quotes): Expr[Int] = '{ println(); 3 }

    def f4(using Quotes): Expr[Int => Int] = '{
      inlineLambda
    }
    println(run(Expr.betaReduce('{$f4($x)})))
    println(withQuotes(Expr.betaReduce('{$f4($x)}).show))
  }

  transparent inline def inlineLambda: Int => Int = x => x + x
}