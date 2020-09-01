import scala.quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    println(foo[Object].show)
    println(bar[Object].show)
  }
  def foo[H : Type](using QuoteContext): Expr[H] = {
    val t = Type[H]
    '{ null.asInstanceOf[t.T] }
  }
  def bar[H : Type](using QuoteContext): Expr[List[H]] = {
    val t = Type[List[H]]
    '{ null.asInstanceOf[t.T] }
  }
}
