class A {
  val bar = () => 5
}

class B(c: Cold[C]) extends A {
  override val bar = () => f      // error
  def f: Int = c.x
}

class C {
  val a: A = new B(this)
  println(a.bar())                // error: touches `x`, not init yet
  new D(a)                        // error: only leak object as `cold` allowed
  val x = 10
}

class D(a: Warm[A]) {
  println(a.bar())                // error: touches `x`, not init yet
}