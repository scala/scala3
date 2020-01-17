import quoted._
import scala.quoted.staging._

object Test {
  given Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {

    val q = '{(qctx: QuoteContext) ?=>
      val a = '{4}
      ${'{(qctx2: QuoteContext) ?=>
        '{${a}}
      }}

    }

    println(q.show)
  }
}
