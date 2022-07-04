// scalajs: --skip --pending

import scala.annotation.static

class Test

object Test {
  @static def test(n: Int): Int =  {
    List(3).map(_ + 2)
    n
  }

  def main(args: Array[String]): Unit = test(3)
}
