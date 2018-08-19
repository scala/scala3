class D(x: Int) {
  class DD {
    rewrite def apply() = new DD()
  }
  val inner = new DD
}
object Test {
  new D(2).inner.apply()
}
