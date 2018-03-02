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
  def foo(ghost x: Int): Int = 42
}

class Bar extends Foo {
  override def foo(ghost x: Int): Int = {
    println("Bar.foo")
    42
  }
}