
import compiletime.uninitialized
object Test {

  class A {
    private[this] var x: String = uninitialized
  }

  class B {
    private[this] var x: String = "good"
    x = "foo"
  }

  class C {
    private[this] var x1: Int = uninitialized
    private[this] var x2: Unit = uninitialized
    private[this] var x3: Char = uninitialized
    private[this] var x4: Boolean = uninitialized
    private[this] var x5: Float = uninitialized
    private[this] var x6: Double = uninitialized
    private[this] var x7: Char = uninitialized
    private[this] var x8: Byte = uninitialized
    private[this] var x9: AnyVal = uninitialized
    private[this] var x10: D = uninitialized
  }

  class D(x: Int) extends AnyVal
}
