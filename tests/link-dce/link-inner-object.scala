import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 19, classesWithReachableMethods = 8, reachableMethods = 43)
  def main(args: Array[String]): Unit = {
    object Foo {
      def test() = 42
    }
    System.out.println(Foo.test())
  }
}
