import scala.annotation.internal

object Test {
  def main(args: Array[String]): Unit = {
    class Foo {
      @internal.link.AssertReachable
      def bar: Int = 42
    }

    val foo = new Foo
    System.out.println(foo.bar)
  }
}
