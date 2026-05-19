class A(val f: () => Int) {
  def mA(p: Int = 0): Int = p
}

trait B {
  def mB(p1: Int): Unit
}

class C[T](val f1: B, val f2: T)

val f = new A(() => {
  val x: B = null
  C[Int](x, 0).f1.mB(1);
  1
}).mA()
