
import dotty.tools.dotc.quoted.Runners._

import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val expr = '{
      val a = 3
      println("foo")
      2 + a
    }
    println(expr.run)
    println(expr.run)
    println(expr.show)

    val lambdaExpr = '{
      (x: Int) => println("lambda(" + x + ")")
    }
    println()

    val lambda = lambdaExpr.run
    lambda(4)
    lambda(5)

    val classExpr = '{
      class A {
        override def toString: String = "Foo"
      }
      new A
    }
    val classExpr2 = '{
      class A {
        override def toString: String = "Bar"
      }
      new A
    }
    println(classExpr.run)
    println(classExpr.run.getClass == classExpr.run.getClass)
    println(classExpr2.run)
    println(classExpr2.run.getClass)
  }
}
