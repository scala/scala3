// Intended to be compiled with either 2.11 or 2.12
class C extends A[B] with B {
  x = 4

  override def f: Int = super.f

  val z = x + f(x) + y
}

object Test extends App {
  println(new C().f)
}
