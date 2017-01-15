import scala.annotation.internal

object Test {

  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 6, reachableMethods = 41)
  def main(args: Array[String]): Unit = ()

  @internal.link.AssertReachable def foo() = System.out.println(42)

  foo()

}
