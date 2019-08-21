import scala.quoted._
import scala.quoted.staging._

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def ret given QuoteContext: Expr[Int => Int] = '{ (x: Int) =>
    ${
      val z = run('{x + 1}) // throws a RunScopeException
      z.toExpr
    }
  }
  def main(args: Array[String]): Unit = {
    try {
      run(ret).apply(10)
      throw new Exception
    } catch {
      case ex: scala.quoted.staging.RunScopeException =>
        // ok
    }
  }
}
