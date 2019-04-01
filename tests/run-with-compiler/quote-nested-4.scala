import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{
      val t = '[String]
      t
    }

    println(q.show)
  }
}
