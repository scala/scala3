import scala.quoted._
import Macros._

object Test {
  def main(args: Array[String]): Unit = {
    val a: Int = blackbox
    val b: 1 = whitebox

    assert(a == 1)
    assert(b == 1)
  }
}
