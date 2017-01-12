import scala.annotation.internal

object Test {

  @internal.link.CallGraphBounds(reachableClasses = 19, classesWithReachableMethods = 5, reachableMethods = 7)
  def main(args: Array[String]): Unit = ()

  @internal.link.AssertReachable def foo() = System.out.println(42)

  foo()

}
