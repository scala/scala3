abstract class A {
  protected def foo(text: String, bar: () => Unit = () => ()): Unit = println(s"$text, $bar")
}

class B extends A {
  def f1(): Unit = {
    super.foo("X")
  }
}
