import scala.quoted.*
import scala.quoted.staging.*

object Test {

  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes {
      val expr = Expr(List(1, 2, 3))
      println(expr.show)
    }
  }

}
