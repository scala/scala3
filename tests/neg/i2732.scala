class B {
  def f = 1
}

trait A { self: B =>
  def g1(x: Int): Int
  def g2 = g1(f)
}

object Test {
  // A should not be a valid SAM type because it's not instantiable
  val x: A = (y: Int) => y + y // error
}
