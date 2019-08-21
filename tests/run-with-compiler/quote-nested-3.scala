import quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

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
