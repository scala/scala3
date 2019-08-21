import quoted._
import scala.quoted.staging._

object Test {
  implicit val toolbox: scala.quoted.staging.Toolbox = scala.quoted.staging.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val q = '{ given (qctx: QuoteContext) => '{3} }
    println(q.show)
  }
}
