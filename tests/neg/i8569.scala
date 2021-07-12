class Outer(x: Int) {
  class Inner(y: Int) {
  }
}
class Outer2(x: Int) {
  class Inner(y: Int) {
  }
}
object Test {
  def outer = Outer(1)
  def outer2 = Outer2(1)

  val x = outer.Inner(2) // error (at posttyper)
}