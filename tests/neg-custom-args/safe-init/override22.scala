trait A {
  val bar: () => Int
}

class B(c: Cold[C]) extends A {
  override val bar = () => f      // error
  def f: Int = c.x
}

class C {
  val a: A = new B(this)
  println(a.bar())                // error: touches `x`, not init yet
  val x = 10
}