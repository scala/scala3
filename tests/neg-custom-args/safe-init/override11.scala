class B(a: Cold[A]) {
  val m: A = a
}

class A {
  val b = new B(this)
  println(b.m.f)          // error

  val x = 10

  @scala.annotation.init
  def f: Int = x
}
