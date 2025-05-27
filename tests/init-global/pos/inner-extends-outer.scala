class Outer {
  val f = 5
  class Inner extends Outer {
    val g = Outer.this.f
  }
}

object O {
  def foo(i: Outer): Unit =
    val i2 = new i.Inner // i2.outer should always be OfClass(Outer)
    foo(i2)

  foo(new Outer)
}
