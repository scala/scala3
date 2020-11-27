sealed trait X
case object Y extends X

object Test {
  def yIs1(proof: Y.type): Int = 1

  def test(x: X) = x match {
    case y: Y.type => yIs1(y)
  }

  def test2(x: X) = x match {
    case (yy: Y.type) as y =>
      yIs1(y)
      yIs1(yy)
  }

  def main(args: Array[String]): Unit = {
    test(Y)

    val a: Nil.type = (Vector(): Any) match {
      case n: Nil.type =>
        assert(false, "should not be reached")
        n
      case _ =>
        Nil
    }
  }
}
