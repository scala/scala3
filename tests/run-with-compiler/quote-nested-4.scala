import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    val q = '{
      val t = '[String]
      t
    }

    println(q.show)
  }
}
