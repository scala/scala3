import quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {

    val q = '{
      val t = '[String]
      t
    }

    println(q.show)
  }
}
