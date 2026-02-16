class Outer(val f: Int) {
  class Inner extends Outer(5) {
    def g(): Int = this.f
  }
}

object O {
  def foo(i: Outer): Unit =
    val i2 = new i.Inner // i2.outer should always be OfClass(Outer)
    println("i2.g = " + i2.g())

  foo(new Outer(6))
}
