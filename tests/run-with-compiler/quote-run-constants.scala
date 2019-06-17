
import scala.quoted.autolift._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    def runAndPrint[T](expr: Expr[T]): Unit = println(run(expr))
    def showAndPrint[T](expr: Expr[T]): Unit = println(show(expr))

    runAndPrint(true)
    runAndPrint('a')
    runAndPrint('\n')
    runAndPrint('"')
    runAndPrint('\'')
    runAndPrint('\\')
    runAndPrint(1)
    runAndPrint(2)
    runAndPrint(3L)
    runAndPrint(4.0f)
    runAndPrint(5.0d)
    runAndPrint("xyz")

    println("======")

    showAndPrint(true)
    showAndPrint('a')
    showAndPrint('\n')
    showAndPrint('"')
    showAndPrint('\'')
    showAndPrint('\\')
    showAndPrint(1)
    showAndPrint(2)
    showAndPrint(3L)
    showAndPrint(4.0f)
    showAndPrint(5.0d)
    showAndPrint("xyz")
    showAndPrint("\n\\\"'")
    showAndPrint("""abc
         xyz""")
  }
}
