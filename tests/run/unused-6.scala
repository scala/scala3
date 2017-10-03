
import dotty.unused

object Test {

  def main(args: Array[String]): Unit = {
    new Foo(foo)
  }

  def foo: Int = {
    println("foo")
    42
  }

}

class Foo(@unused a: Int) {
  println("Foo")
}
