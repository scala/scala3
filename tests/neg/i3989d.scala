trait A[+_X] {
  protected[this] type X = _X // error: variance
  def f: X
}

trait B extends A[B] {
  def f: X = new B {}
}

class C extends B with A[C] {
  // should be required because the inherited f is of type B, not C
  // override def f: X = new C
}

object Test extends App {
  val c1 = new C
  val c2: C = c1.f
}
