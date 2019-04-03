
import scala.quoted.autolift._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    def run[T](expr: Expr[T]): Unit = println(expr.run)
    def show[T](expr: Expr[T]): Unit = println(expr.show)

    run(true)
    run('a')
    run('\n')
    run('"')
    run('\'')
    run('\\')
    run(1)
    run(2)
    run(3L)
    run(4.0f)
    run(5.0d)
    run("xyz")

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
