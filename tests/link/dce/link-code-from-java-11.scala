import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 20, classesWithReachableMethods = 7, reachableMethods = 8)
  def main(args: Array[String]): Unit = {
    System.out.println((new Foo): Object)
  }

}

class Foo {
  @internal.link.AssertReachable
  override def toString: String = "Foo.toString"
}
