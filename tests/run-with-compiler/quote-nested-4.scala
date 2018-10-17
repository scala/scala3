import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    println(tb.show {
      val q = '{
        val t = '[String]
        t
      }
      q
    })
  }
}
