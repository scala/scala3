//> using options -source:3.0-migration

// Hope to see a migration warning! with quickfix.

class P {
  def g(): Int = 42
}
class C extends P {
  def g(i: Int): Int = 42 + i
}

object Test {
  val over = Over()
  println(over.f) // nowarn Java
  val c = C()
  println(c.g) // warn migration
}
