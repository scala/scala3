object Test {
  import scalatest._

  class Box(val x: Int) {
    def >(y: Int): Boolean = x > y
    def >(b: Box): Boolean = x > b.x
  }

  def main(args: Array[String]): Unit = {
    val a: Int = 100
    assert(a > 5)
    assert(a > 5.0)
    assert(a > 'a')

    val b1 = new Box(10)
    val b2 = new Box(3)
    assert(b1 > 4)
    assert(b1 > b2)
  }
}
