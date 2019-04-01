import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

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
