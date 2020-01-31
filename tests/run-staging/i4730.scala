import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def ret(given QuoteContext): Expr[Int => Int] = '{ (x: Int) =>
    ${
      val z = run('{x + 1}) // throws a RunScopeException
      Expr(z)
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
