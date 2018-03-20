import quoted._
import dotty.tools.dotc.quoted.Toolbox._

object Test {
  def main(args: Array[String]): Unit = {

    val q = '{
      type T = String
      val x = "foo"
      ~{
        val y = '(x)
        '{ val z: T = ~y }
      }
      x
    }

    println(q.show)
  }
}
