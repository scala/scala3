import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def ret(using QuoteContext): Expr[Int => Int] = '{ (x: Int) =>
    ${
      val z = run('{x + 1}) // throws scala.quoted.internal.ScopeException =>
      Expr(z)
    }
  }
  def main(args: Array[String]): Unit = {
    scala.mytest.myTest()
  }
}

package scala {
  package mytest {
    def myTest()(using Toolbox) = {
      try {
        run(Test.ret).apply(10)
        throw new Exception
      } catch {
        case ex: Exception if ex.getClass.getName == "scala.quoted.internal.ScopeException" =>
          // ok
      }
    }
  }
}
