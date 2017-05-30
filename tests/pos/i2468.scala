
object Test {

  class A {
    private[this] var x: String = _
  }

  class B {
    private[this] var x: String = _
    x = "foo"
  }

  class C {
    private[this] var x1: Int = _
    private[this] var x2: Unit = _
    private[this] var x3: Char = _
    private[this] var x4: Boolean = _
    private[this] var x5: Float = _
    private[this] var x6: Double = _
    private[this] var x7: Char = _
    private[this] var x8: Byte = _
    private[this] var x9: AnyVal = _
    private[this] var x10: D = _
  }

  class D(x: Int) extends AnyVal
}