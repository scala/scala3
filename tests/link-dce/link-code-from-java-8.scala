import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 6, reachableMethods = 8)
  def main(args: Array[String]): Unit = {
    System.out.println(new Foo)
  }

}

class Foo {
  @internal.link.AssertReachable
  override def toString: String = "Foo.toString"
}
