import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val a: Int = blackbox
    val b: 1 = whitebox

    assert(a == 1)
    assert(b == 1)
  }

  inline def blackbox: Int = 1

  transparent inline def whitebox: Int = 1

}
