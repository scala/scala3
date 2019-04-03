
import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val lambdaExpr = '{
      (x: Int) => println("lambda(" + x + ")")
    }
    println()

    val lambda = lambdaExpr.run
    lambda(4)
    lambda(5)
  }
}
