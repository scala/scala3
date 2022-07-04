// scalajs: --skip

class A {
  def foo1[T]: Unit = {}
  def foo2[T](): Unit = {}
  def foo3[T]()(): Unit = {}
  def foo4: Unit = {}
  def foo5(): Unit = {}
  def foo6()(): Unit = {}
}

object Test {
  def main(args: Array[String]): Unit = {
    classOf[A].getMethods.toList.filter(_.getName.startsWith("foo")).foreach { m =>
      assert(m.getGenericReturnType == Void.TYPE, s"Method does not return void: `${m}`")
    }
  }
}