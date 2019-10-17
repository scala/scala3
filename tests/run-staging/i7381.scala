import scala.quoted._
import scala.quoted.staging._

object Test {

  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    withQuoteContext {
      val expr = Expr(List(1, 2, 3))
      println(expr.show)
    }
  }

}
