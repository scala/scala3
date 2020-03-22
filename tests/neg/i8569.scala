class Outer(x: Int) {
  class Inner(y: Int) {
  }
}
object Test {
  def outer = Outer(1)

  outer.Inner(2) // error
  new outer.Inner(2) // error
}