import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 44, classesWithReachableMethods = 9, reachableMethods = 59)
  def main(args: Array[String]): Unit = {
    println(new Foo) // In this test case the standard lib is compiled separately.
  }

}

class Foo {
  @internal.link.AssertReachable
  override def toString: String = "Foo.toString"
}
