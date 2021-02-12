package paramlists

class Meter[T](val x: T) extends AnyVal {
  def zero: T = x
  def zero2[M >: T]: M = x
  def one(): T = x
  def one2[M >: T](): M = x
  def one3(x: T): T = x
  def one4[M >: T](x: M): M = x
  def two(x: T)(y: T): T = y
  def two2[M >: T](x: T)(y: M): M = y
}

object Test {
  def test: Unit = {
    val m1 = new Meter(1)
    m1.zero
    m1.zero2
    m1.one()
    m1.one2()
    m1.one3(10)
    m1.two(11)(12)
    m1.two2(11)(12)

    {
      import m1.*

      zero
      zero2
      one()
      one2()
      one3(10)
      two(11)(12)
      two2(11)(12)
    }
  }
}
