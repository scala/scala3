import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 19, classesWithReachableMethods = 7, reachableMethods = 42)
  def main(args: Array[String]): Unit = {
    class Foo {
      @internal.link.AssertReachable
      def bar: Int = 42
    }

    val foo = new Foo
    System.out.println(foo.bar)
  }
}
