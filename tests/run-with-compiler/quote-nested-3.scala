import quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = withQuoteContext {
    val q = '{
      type T = String
      val x = "foo"
      ${
        val y = 'x
        '{ val z: T = $y }
      }
      x
    }

    println(q.show)
  }
}
