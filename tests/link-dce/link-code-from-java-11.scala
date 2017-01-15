import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 8, reachableMethods = 42)
  def main(args: Array[String]): Unit = {
    System.out.println((new Foo): Object)
  }

}

class Foo {
  @internal.link.AssertReachable
  override def toString: String = "Foo.toString"
}
