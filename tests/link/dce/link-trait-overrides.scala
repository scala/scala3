import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    new Foo().foo
  }
}

class Foo extends Bar

trait Bar extends Baz {
  override def foo = System.out.println(42)
}

trait Baz {
  @internal.link.AssertNotReachable
  def foo = System.out.println(5)
}
