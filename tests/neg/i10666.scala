class A
class B extends A

trait Foo {
  def foo[T <: B](tx: T): Unit
}

class Bar extends Foo { // error
  def foo[T <: A](tx: T): Unit = {}
}

object Test {
  def main(args: Array[String]): Unit = {
    val f: Foo = new Bar
    f.foo(new B)
  }
}
