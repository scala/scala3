import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    new Foo()
  }
}

class Foo(n: Int) {
  def this() = this(42)

  @internal.link.AssertReachable def foo() = System.out.println(n)
  foo()
}
