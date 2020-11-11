import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {

    val q = '{ (using qctx: QuoteContext) =>
      val t = Type.of[String]
      t
    }

    println(q.show)
  }
}
