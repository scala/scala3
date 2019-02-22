import quoted._
import scala.quoted.Toolbox.Default._

object Test {
  def main(args: Array[String]): Unit = {
    val q = '{ '{3} }
    println(q.show)
  }
}
