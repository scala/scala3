trait T {
  def bar(): Int
}

class C extends T {
  def bar(): Int = 5
}

class Outer(val t: T) {
  class Inner extends Outer(new C) {
    def g(): Int = Outer.this.t.bar()
  }
}

object O extends T {
  def foo(i: Outer): Int =
    val i2 = new i.Inner // i2.outer should always be OfClass(Outer)
    i2.g()

  val f1 = foo(new Outer(this))
  val f2 = 5
  def bar(): Int = f2 // warn
}