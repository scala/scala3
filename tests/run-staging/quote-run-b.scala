
import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def lambdaExpr(using Quotes) = '{
      (x: Int) => println("lambda(" + x + ")")
    }
    println()

    val lambda = run(lambdaExpr)
    lambda(4)
    lambda(5)
  }
}
