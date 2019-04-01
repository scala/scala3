import quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{
      val a = '{4}
      '{${a}}
    }

    println(q.show)
  }
}
