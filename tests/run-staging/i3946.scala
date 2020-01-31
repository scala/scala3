import scala.quoted._
import scala.quoted.staging._
object Test {
  def main(args: Array[String]): Unit = {
    given Toolbox = Toolbox.make(getClass.getClassLoader)
    def u(given QuoteContext): Expr[Unit] = '{}
    println(withQuoteContext(u.show))
    println(run(u))
  }
}
