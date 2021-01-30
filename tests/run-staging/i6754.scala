
import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit =
    scala.MyTest.myTest()
}

package scala {
  object MyTest {
    implicit val tbx: scala.quoted.staging.Compiler = scala.quoted.staging.Compiler.make(getClass.getClassLoader)

    def myTest() = {
      def y(using Quotes): Expr[Unit] = '{
        def x(using Quotes): Expr[Unit] = '{println("bar")}
        println("foo")
        run(x)
      }
      try {
        run(y)
        throw new Exception
      } catch {
        case ex: java.lang.reflect.InvocationTargetException =>
          assert(ex.getTargetException.getClass.getName == "scala.quoted.runtime.impl.ScopeException")
      }
    }
  }
}
