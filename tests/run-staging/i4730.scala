import scala.quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
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
      case ex: scala.quoted.ScopeException =>
        // ok
    }
  }
}
