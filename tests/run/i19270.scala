// scalajs: --skip

trait T {
  def foo(x: Int): Int = x + 1
}

class C extends T

object Test {
  def main(args: Array[String]): Unit = {
    println("i19270")
    val m = classOf[C].getDeclaredMethod("foo", classOf[Int])
    assert(m.getDeclaringClass() == classOf[C], m.getDeclaringClass())
    assert(!m.isBridge(), "foo should not have the ACC_BRIDGE flag")
    assert(!m.isSynthetic(), "foo should not have the ACC_SYNTHETIC flag")
  }
}
