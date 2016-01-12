class X {
  def f(x: AnyRef) = x.toString
  def f(x: AnyRef, y: AnyRef*) = y.mkString(x.toString)
}

class Y {
  def f(x: Int) = x.toString
  def f(x: Int, y: Int*) = y.mkString(x.toString)
}

object Test {
  val x: AnyRef = "a"
  val res0 = new X
  res0.f(x)
  val res1 = new Y
  res1.f(5)
}
