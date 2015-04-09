abstract class X[T]{
  def go2(x:T)(y:T = x): T = y
  def go: T
  def go1(x: T) = x
}

class Y extends X[Int] {
  override def go2(x: Int)(z: Int) = 2
  override def go = 0
  override def go1(x: Int) = x
}
