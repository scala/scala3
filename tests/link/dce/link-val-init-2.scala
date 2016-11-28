import scala.annotation.internal

class Foo {
  @internal.link.AssertReachable
  def foo = System.out.println(42)

  {
    val bar = foo
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    new Foo
  }
}
