import quoted._
import scala.quoted.Toolbox.Default._

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{
      val t = '[String]
      t
    }

    println(q.show)
  }
}
