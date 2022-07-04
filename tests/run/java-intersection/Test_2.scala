// scalajs: --skip

import java.io.Serializable

class Sub extends A_1 {
  override def foo[T <: Object & Serializable](x: T) =
    super.foo(x)
    println("Sub1")

  override def foo[T <: Cloneable & Serializable](x: T) =
    super.foo(x)
    println("Sub2")
}

object Test {
  def main(args: Array[String]): Unit = {
    val x: Object & Serializable = new Serializable {}
    val y: Cloneable & Serializable = new Cloneable with java.io.Serializable {}

    val a = new A_1
    a.foo(x)
    a.foo(y)

    val s = new Sub
    s.foo(x)
    s.foo(y)
  }
}
