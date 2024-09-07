//> using options -deprecation

class C {
  def f = 42 + (x = 1) // warn
  def multi(x: Int, y: Int): Int = x + y
  def g = new C() `multi` (x = 42, y = 27) // warn // warn
}
