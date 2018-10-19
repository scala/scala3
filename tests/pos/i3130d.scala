class D(x: Int) {
  class DD {
    inline def apply() = new DD()
  }
  val inner = new DD
}
object Test {
  new D(2).inner.apply()
}
