package privatethisparam

class Meter[T](x: T) extends AnyVal {
  def zero: T = x
}

class Meter2(private[this] val x: Int) extends AnyVal {
  def foo = x
}

object Test {
  def bar = new Meter2(42)
  def useZero = new Meter(5).zero
  def test: Unit = {
    val m1 = new Meter(1)
    m1.zero
  }
}