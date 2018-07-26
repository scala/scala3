class D(x: Int) {
  class DD {
    transparent def apply() = new DD()
  }
  val inner = new DD
}
object Test {
  new D(2).inner.apply()
}
