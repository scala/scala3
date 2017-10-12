object Test {

  def main(args: Array[String]): Unit = {
    new Bar().foo(foo)
  }

  def foo = {
    println("foo")
    42
  }
}

class Foo {
  def foo(unused x: Int): Int = 42
}

class Bar extends Foo {
  override def foo(unused x: Int): Int = {
    println("Bar.foo")
    42
  }
}