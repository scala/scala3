
import scala.quoted.autolift._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    def runAndPrint[T](expr: Expr[T]): Unit = println(run(expr))
    def show[T](expr: Expr[T]): Unit = println(expr.show)

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

    show(true)
    show('a')
    show('\n')
    show('"')
    show('\'')
    show('\\')
    show(1)
    show(2)
    show(3L)
    show(4.0f)
    show(5.0d)
    show("xyz")
    show("\n\\\"'")
    show("""abc
         xyz""")
  }
}
