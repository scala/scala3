import Macros._
object Test {
  def main(args: Array[String]): Unit = {
    val x = 1
    assert2(x != 0)
    assert2(x == 0)
  }
}
