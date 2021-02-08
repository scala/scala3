

import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def runAndPrint[T](expr: Quotes ?=> Expr[T]): Unit = println(run(expr))

    runAndPrint(Expr(true))
    runAndPrint(Expr('a'))
    runAndPrint(Expr('\n'))
    runAndPrint(Expr('"'))
    runAndPrint(Expr('\''))
    runAndPrint(Expr('\\'))
    runAndPrint(Expr(1))
    runAndPrint(Expr(2))
    runAndPrint(Expr(3L))
    runAndPrint(Expr(4.0f))
    runAndPrint(Expr(5.0d))
    runAndPrint(Expr("xyz"))

    println("======")

    withQuotes {
      def show[T](expr: Expr[T]): Unit = println(expr.show)

      show(Expr(true))
      show(Expr('a'))
      show(Expr('\n'))
      show(Expr('"'))
      show(Expr('\''))
      show(Expr('\\'))
      show(Expr(1))
      show(Expr(2))
      show(Expr(3L))
      show(Expr(4.0f))
      show(Expr(5.0d))
      show(Expr("xyz"))
      show(Expr("\n\\\"'"))
      show(Expr(
        """abc
         xyz"""))
    }
  }
}
