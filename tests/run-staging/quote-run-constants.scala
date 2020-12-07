

import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def runAndPrint[T](expr: Quotes ?=> Expr[T]): Unit = println(run(expr))

    runAndPrint(Value(true))
    runAndPrint(Value('a'))
    runAndPrint(Value('\n'))
    runAndPrint(Value('"'))
    runAndPrint(Value('\''))
    runAndPrint(Value('\\'))
    runAndPrint(Value(1))
    runAndPrint(Value(2))
    runAndPrint(Value(3L))
    runAndPrint(Value(4.0f))
    runAndPrint(Value(5.0d))
    runAndPrint(Value("xyz"))

    println("======")

    withQuotes {
      def show[T](expr: Expr[T]): Unit = println(expr.show)

      show(Value(true))
      show(Value('a'))
      show(Value('\n'))
      show(Value('"'))
      show(Value('\''))
      show(Value('\\'))
      show(Value(1))
      show(Value(2))
      show(Value(3L))
      show(Value(4.0f))
      show(Value(5.0d))
      show(Value("xyz"))
      show(Value("\n\\\"'"))
      show(Value(
        """abc
         xyz"""))
    }
  }
}
