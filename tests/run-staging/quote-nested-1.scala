import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val q = '{ (given qctx: QuoteContext) => '{3} }
    println(q.show)
  }
}
