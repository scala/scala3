import scala.quoted._
import scala.quoted.staging._

object Test {

  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes {
      val expr = Expr(List(1, 2, 3))
      println(expr.show)
    }
  }

}
