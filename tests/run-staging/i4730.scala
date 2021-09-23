import scala.quoted.*
import scala.quoted.staging.*


package i4730:
  given Compiler = Compiler.make(getClass.getClassLoader)
  def ret(using Quotes): Expr[Int => Int] = '{ (x: Int) =>
    ${
      val z = run('{x + 1}) // throws scala.quoted.runtime.impl.ScopeException =>
      Expr(z)
    }
  }

package scala {
  package mytest {
    def myTest()(using Compiler) = {
      try {
        run(i4730.ret).apply(10)
        throw new Exception
      } catch {
        case ex: Exception if ex.getClass.getName == "scala.quoted.runtime.impl.ScopeException" =>
          // ok
      }
    }
  }
}
object Test {
  import i4730.given
  def main(args: Array[String]): Unit = {
    scala.mytest.myTest()
  }
}
