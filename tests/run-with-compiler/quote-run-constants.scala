
import scala.quoted.Toolbox.Default._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make

    def run[T](expr: Expr[T]): Unit = println(expr.run)
    def show[T](expr: Expr[T]): Unit = println(expr.show)

    run(true.toExpr)
    run('a'.toExpr)
    run('\n'.toExpr)
    run('"'.toExpr)
    run('\''.toExpr)
    run('\\'.toExpr)
    run(1.toExpr)
    run(2.toExpr)
    run(3L.toExpr)
    run(4.0f.toExpr)
    run(5.0d.toExpr)
    run("xyz".toExpr)

    println("======")

    show(true.toExpr)
    show('a'.toExpr)
    show('\n'.toExpr)
    show('"'.toExpr)
    show('\''.toExpr)
    show('\\'.toExpr)
    show(1.toExpr)
    show(2.toExpr)
    show(3L.toExpr)
    show(4.0f.toExpr)
    show(5.0d.toExpr)
    show("xyz".toExpr)
    show("\n\\\"'".toExpr)
    show("""abc
         xyz""".toExpr)
  }
}
