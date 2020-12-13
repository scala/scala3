
import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit =
    scala.MyTest.myTest()
}

package scala {
  object MyTest {
    implicit val tbx: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)

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
