import quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {

    val q = '{ given (qctx: QuoteContext) =>
      val a = '{4}
      ${'{ given (qctx2: QuoteContext) =>
        '{${a}}
      }}

    }

    println(q.show)
  }
}
