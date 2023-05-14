// scalajs: --skip

class Wrapper(val value: Int)

abstract class Foo(val x: Int)

class Test {
  def foo(wrapper: Wrapper): Unit = {
    new Foo(wrapper.value) {}
  }
}
object Test extends App {
  def foo(wrapper: Wrapper): Foo =
    new Foo(wrapper.value) {}
  def printFields(obj: Any) =
    println(obj.getClass.getDeclaredFields.map(_.toString).sorted.toList.mkString("\n"))
  printFields(foo(new Wrapper(1)))
}
